% LUNAR LANDER GAME in SWI-Prolog
% Translation of Jim Storer's FOCAL lunar lander game to Prolog

% This translation was created by using Claude Code to translate
% <https://github.com/kristopherjohnson/lunar-c/blob/master/lunar.c>
% to Prolog.

:- use_module(library(readutil)).

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
    game_loop(120, 1, 32500, 16500, 0.001, 1.8, 0).

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
        AltMiles is truncate(A),
        AltFeet is truncate(5280 * (A - AltMiles)),
        VelMPH is 3600 * V,
        FuelLbs is M - N,

        format('~t~w~7|~t~w~23|~t~w~30|~t~2f~45|~t~1f~57|      ', [L, AltMiles, AltFeet, VelMPH, FuelLbs]),

        % Get fuel rate input
        prompt_for_k(K),

        % Process 10-second turn
        process_turn(A, V, M, N, G, Z, L, K)
    ).

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
            BurnTime is FuelRemaining / K,
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
        ActualT is min(MaxT, TimeToSurface),
        apply_physics_simple(A, V, M, G, Z, K, ActualT, NewA, NewV, NewM)
    ).

% Simplified physics for very short time periods
apply_physics_simple(A, V, M, G, Z, K, T, NewA, NewV, NewM) :-
    AccelGrav is G,
    AccelThrust is -Z * K / M,
    TotalAccel is AccelGrav + AccelThrust,

    NewV is V + TotalAccel * T,
    NewA is A - V * T - 0.5 * TotalAccel * T * T,
    NewM is M - K * T.

% Handle fuel depletion
fuel_out(A, V, G, L, M, N) :-
    format('FUEL OUT AT ~2f SECS~n', [L]),
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
    format('ON THE MOON AT ~2f SECS~n', [L]),
    ImpactVel is abs(3600 * V),
    format('IMPACT VELOCITY OF ~2f M.P.H.~n', [ImpactVel]),
    FuelLeft is M - N,
    format('FUEL LEFT: ~2f LBS~n', [FuelLeft]),

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
        format('IN FACT YOU BLASTED A NEW LUNAR CRATER ~2f FT. DEEP~n', [CraterDepth])
    ), nl.

% Prompt for fuel rate with validation
prompt_for_k(K) :-
    write('K=:'),
    read_line_to_string(user_input, Line),
    % Remove trailing newline if present
    (sub_string(Line, _, 1, 0, '\n') ->
        sub_string(Line, 0, _, 1, CleanLine)
    ;
        CleanLine = Line
    ),
    (CleanLine = "" ->
        % Empty line, prompt again
        write('NOT POSSIBLE'),
        write_dots(51),
        prompt_for_k(K)
    ;
        catch(
            (number_string(K, CleanLine),
             (K >= 0, (K =:= 0; (K >= 8, K =< 200)) ->
                 true
             ;
                 fail
             )
            ),
            _,
            fail
        ) ->
        true
    ;
        write('NOT POSSIBLE'),
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
    write('(ANS. YES OR NO):'),
    read_line_to_string(user_input, Line),
    (Line = "" ->
        accept_yes_or_no(Answer)
    ;
        string_chars(Line, [First|_]),
        char_code(First, Code),
        (memberchk(Code, [89, 121]) ->  % 'Y' or 'y'
            Answer = yes
        ; memberchk(Code, [78, 110]) ->  % 'N' or 'n'
            Answer = no
        ;
            accept_yes_or_no(Answer)
        )
    ).

% Start the game when file is loaded
:- initialization(lunar_lander).
