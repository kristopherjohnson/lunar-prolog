% Translation of
% <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
% by Jim Storer from FOCAL to C.

% This translation was created by using Claude Code and Google Gemini to
% translate <https://github.com/kristopherjohnson/lunar-c/blob/master/lunar.c>
% to Prolog.

% Main entry point
lunar_lander :-
    write('CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY'), nl,
    write('YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE'), nl,
    write('BETWEEN 8 & 200 LBS/SEC. YOU\'VE 16000 LBS FUEL. ESTIMATED'), nl,
    write('FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS'), nl, nl, nl,
    play_game_loop.

% Game loop with replay option
play_game_loop :-
    play_game,
    nl, nl, nl,
    write('TRY AGAIN?'), nl,
    accept_yes_or_no(Answer),
    (Answer = yes -> play_game_loop ; true),
    write('CONTROL OUT'), nl, nl,
    halt.

% Main game logic
play_game :-
    write('FIRST RADAR CHECK COMING UP'), nl, nl,
    write('COMMENCE LANDING PROCEDURE'), nl,
    write('TIME,SECS   ALTITUDE,MILES+FEET   VELOCITY,MPH   FUEL,LBS   FUEL RATE'), nl,

    % Initialize game state
    game_loop(120.0, 1.0, 32500.0, 16500.0, 0.001, 1.8, 0.0).

% A - Altitude (miles)
% G - Gravity
% I - Intermediate altitude (miles)
% J - Intermediate velocity (miles/sec)
% K - Fuel rate (lbs/sec)
% L - Elapsed time (sec)
% M - Total weight (lbs)
% N - Empty weight (lbs, Note: M - N is remaining fuel weight)
% S - Time elapsed in current 10-second turn (sec)
% T - Time remaining in current 10-second turn (sec)
% V - Downward speed (miles/sec)
% W - Temporary working variable
% Z - Thrust per pound of fuel burned

% Main game turn loop
game_loop(A, V, M, N, G, Z, L) :-
    % Check if landed
    (A =< 0 ->
        on_the_moon(V, L, M, N)
    ;
        % Display current status
        display_status(L, A, V, M, N, K),

        % Get fuel rate input
        prompt_for_k(K),

        % Process 10-second turn
        process_turn(A, V, M, N, G, Z, L, K)
    ).

% Write time, altitude, velocity, and fuel remaining.
display_status(L, A, V, M, N, K) :-
    L_int is round(L),
    write(' '), write_rjust(L_int, 6),
    AltMiles is truncate(A),
    AltFeet is truncate(5280 * (A - AltMiles)),
    write(' '), write_rjust(AltMiles, 15),
    write(' '), write_rjust(AltFeet, 6),
    VelMPH is 3600 * V,
    write(' '), write_fjust(VelMPH, 14, 2),
    FuelLbs is M - N,
    write(' '), write_fjust(FuelLbs, 11, 1),
    write('      K=:'),
    flush_output.

% Process a single 10-second turn
process_turn(A, V, M, N, G, Z, L, K) :-
    % Check fuel remaining
    FuelRemaining is M - N,
    (FuelRemaining < 0.001 ->
        fuel_out(A, V, G, L, M, N)
    ;
        % Calculate fuel consumption for 10 seconds
        FuelToUse is K * 10,
        (FuelToUse > FuelRemaining ->
            % Partial burn - use all remaining fuel
            (K > 0 -> BurnTime is FuelRemaining / K ; BurnTime is 0),
            apply_physics(A, V, M, G, Z, K, BurnTime, NewA, NewV, NewM),
            NewL is L + BurnTime,
            fuel_out(NewA, NewV, G, NewL, NewM, N)
        ;
            % Full 10-second burn
            apply_physics(A, V, M, G, Z, K, 10, NewA, NewV, NewM),
            NewL is L + 10,
            game_loop(NewA, NewV, NewM, N, G, Z, NewL)
        )
    ).

% Apply physics for given time period
apply_physics(A, V, M, G, Z, K, T, NewA, NewV, NewM) :-
    % If we're very close to surface, use precise landing calculation
    (A =< 0.1 ->
        precise_landing(A, V, M, G, Z, K, T, NewA, NewV, NewM)
    ;
        % Normal physics calculation
        Q is T * K / M,
        Q2 is Q * Q,
        Q3 is Q2 * Q,
        Q4 is Q3 * Q,
        Q5 is Q4 * Q,

        NewV is V + G * T + Z * (-Q - Q2/2 - Q3/3 - Q4/4 - Q5/5),
        NewA is A - G * T * T / 2 - V * T + Z * T * (Q/2 + Q2/6 + Q3/12 + Q4/20 + Q5/30),
        NewM is M - T * K
    ).

% Precise landing calculation for final approach
precise_landing(A, V, M, G, Z, K, MaxT, NewA, NewV, NewM) :-
    (A =< 0 ->
        NewA = 0, NewV = V, NewM = M
    ;
        % Calculate time to reach surface
        TimeToSurface is 2 * A / (V + sqrt(V * V + 2 * A * (G - Z * K / M))),
        (MaxT < TimeToSurface -> ActualT = MaxT ; ActualT = TimeToSurface),
        apply_physics_simple(A, V, M, G, Z, K, ActualT, NewA, NewV, NewM)
    ).

% Simplified physics for very short time periods
apply_physics_simple(A, V, M, G, Z, K, T, NewA, NewV, NewM) :-
    AccelGrav is G,
    (M =< 0 -> AccelThrust = 0 ; AccelThrust is -Z * K / M),
    TotalAccel is AccelGrav + AccelThrust,

    NewV is V + TotalAccel * T,
    NewA is A - V * T - 0.5 * TotalAccel * T * T,
    NewM is M - K * T.

% Handle fuel depletion
fuel_out(A, V, G, L, M, N) :-
    write('FUEL OUT AT '), write_float(L, 2), write(' SECS'), nl,
    % Free fall calculation
    (A > 0 ->
        TimeToSurface is (sqrt(V * V + 2 * A * G) - V) / G,
        FinalV is V + G * TimeToSurface,
        FinalL is L + TimeToSurface
    ;
        FinalV = V,
        FinalL = L
    ),
    on_the_moon(FinalV, FinalL, M, N).

% Handle moon landing
on_the_moon(V, L, M, N) :-
    write('ON THE MOON AT '), write_float(L, 2), write(' SECS'), nl,
    ImpactVel is abs(3600 * V),
    write('IMPACT VELOCITY OF '), write_float(ImpactVel, 2), write(' M.P.H.'), nl,
    FuelLeft is M - N,
    write('FUEL LEFT: '), write_float(FuelLeft, 2), write(' LBS'), nl,

    % Evaluate landing quality
    (ImpactVel =< 1 ->
        write('PERFECT LANDING !-(LUCKY)')
    ; ImpactVel =< 10 ->
        write('GOOD LANDING-(COULD BE BETTER)')
    ; ImpactVel =< 22 ->
        write('CONGRATULATIONS ON A POOR LANDING')
    ; ImpactVel =< 40 ->
        write('CRAFT DAMAGE. GOOD LUCK')
    ; ImpactVel =< 60 ->
        write('CRASH LANDING-YOU\'VE 5 HRS OXYGEN')
    ;
        write('SORRY,BUT THERE WERE NO SURVIVORS-YOU BLEW IT!'), nl,
        CraterDepth is ImpactVel * 0.277777,
        write('IN FACT YOU BLASTED A NEW LUNAR CRATER '), write_float(CraterDepth, 2), write(' FT. DEEP'), nl
    ), nl.

% Prompt for fuel rate with validation
prompt_for_k(K) :-
    read_line(Codes),
    ( Codes = [] ->
        write('NOT POSSIBLE'), nl,
        write_dots(51),
        prompt_for_k(K)
    ;
      catch(
          ( number_codes(K, Codes),
            (K >= 0, (K =:= 0; (K >= 8, K =< 200)))
          ),
          _,
          fail
      ) ->
        true
    ;
        write('NOT POSSIBLE'), nl,
        write_dots(51),
        prompt_for_k(K)
    ).

% Helper to write dots for error message
write_dots(0) :- nl, !.
write_dots(N) :-
    N > 0,
    write('.'),
    N1 is N - 1,
    write_dots(N1).

% Accept yes/no input
accept_yes_or_no(Answer) :-
    write('(ANS. YES OR NO):'), flush_output,
    read_line(Codes),
    ( Codes = [] ->
        accept_yes_or_no(Answer)
    ;
        Codes = [First|_],
        ( member(First, [89, 121]) -> % 'Y' or 'y'
            Answer = yes
        ; member(First, [78, 110]) -> % 'N' or 'n'
            Answer = no
        ;
            accept_yes_or_no(Answer)
        )
    ).

% Helper to write spaces
write_spaces(0) :- !.
write_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    write_spaces(N1).

% write_rjust(N, W)
% Write integer N right-justified in a field of W characters.
write_rjust(N, W) :-
    number_codes(N, Codes),
    length(Codes, Len),
    Padding is W - Len,
    ( Padding > 0 -> write_spaces(Padding) ; true ),
    write(N).

% write_fjust(Float, Width, Precision)
% Write float Float right-justified in a field of Width characters,
% with Precision digits after the decimal point.
write_fjust(Float, Width, Precision) :-
    get_float_codes(Float, Precision, Codes),
    length(Codes, Len),
    Padding is Width - Len,
    ( Padding > 0 -> write_spaces(Padding) ; true ),
    forall(member(Code, Codes), put_code(Code)).

% write_float(Float, Precision)
% Write float Float with Precision digits after the decimal point.
write_float(Float, Precision) :-
    get_float_codes(Float, Precision, Codes),
    forall(member(Code, Codes), put_code(Code)).

% get_float_codes(Float, Precision, Codes)
% Convert a float to a list of character codes with a given precision.
get_float_codes(Float, Precision, Codes) :-
    Multiplier is round(10**Precision),
    Value is round(Float * Multiplier),
    Integer is Value // Multiplier,
    Fractional is abs(Value mod Multiplier),
    number_codes(Integer, IntegerCodes),
    ( Precision > 0 ->
        number_codes(Fractional, FracCodesRaw),
        length(FracCodesRaw, FracLen),
        NumZeros is Precision - FracLen,
        ( NumZeros > 0 ->
            findall(0'0, between(1, NumZeros, _), Zeros),
            append(Zeros, FracCodesRaw, FracCodesPadded)
        ;
            FracCodesPadded = FracCodesRaw
        ),
        append(IntegerCodes, [0'.|FracCodesPadded], Codes)
    ;
        Codes = IntegerCodes
    ).

% Read a line of character codes from input
read_line(Codes) :-
    get_code(C),
    read_line_codes(C, Codes).

read_line_codes(-1, []) :- !.
read_line_codes(10, []) :- !.
read_line_codes(13, []) :- get_code(10), !. % CR LF
read_line_codes(13, []) :- !. % CR
read_line_codes(C, [C|Cs]) :-
    C >= 0, % valid code
    get_code(NextC),
    read_line_codes(NextC, Cs).

% Start the game when file is loaded
:- initialization(lunar_lander).
go :- lunar_lander.
