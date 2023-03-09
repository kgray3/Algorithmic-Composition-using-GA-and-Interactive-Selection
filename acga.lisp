; File: acga.lisp
; Hybridized constraint system and genetic algorithm for
; user-interactive algorithmic composition.

( setf *beat-total* 8 )
; Global var for current constraint array we are woring with
( setf *constraint-arr* '() )


; Method to generate a random melody based on the constraints
; of the first melody in the proposal
; note: use format t for EasyABC display method :)
( defmethod generate-melody1 ()
    ( setf *constraint-arr* *melody-durations* )
    ( setf dlist ( generate-durations '() ) )
    ( setf plist ( generate-pitches dlist ) )
    ( generate-notes dlist plist '() ( select-random-arr-element *melody-octaves* ) ) 
)

; Method to generate melody2 as either a harmonization,
; permutation, or random melody
( defmethod generate-melody2 ( ( m1-notes list ) )
    ( setf choice ( random 3 ) )
    (cond
        (( = choice 0 )
            ( create-harmonization m1-notes )
        )
        (( = choice 1 )
            ; permutation
            ( list-permutation m1-notes )
        )
        (( = choice 2 )
            ;random melody
            ( generate-melody1 )
        )
    )

)

; Method to create a harmony given a list of notes
( defmethod create-harmonization ( ( notes-list list ) )
    (cond
        (( = 0 ( length notes-list ) )
            '()
        )
        (t
            ( setf current-pitch ( note-pitch ( car notes-list ) ) )
            ( setf current-pitch-interval ( position current-pitch *CMAJOR* ) )
            (cond
                (( > ( + current-pitch-interval 2 ) ( - (length *CMAJOR*) 1 ) )
                    ( setf new-pitch ( nth ( - current-pitch-interval 3 ) *CMAJOR* ) )
                    ( setf new-note 
                        ( make-instance 'note
                            :pitch new-pitch
                            :duration ( note-duration ( car notes-list ) )
                            :octave ( note-octave ( car notes-list ) )
                            :str-representation ( generate-str-representation new-pitch ( note-duration ( car notes-list ) ) ( note-octave ( car notes-list ) ) )
                        )
                    )
                    ( cons new-note ( create-harmonization ( cdr notes-list ) ) )
                )
                (t
                    ( setf new-pitch ( nth ( + current-pitch-interval 2 ) *CMAJOR* ) )
                    ( setf new-note 
                        ( make-instance 'note
                            :pitch new-pitch
                            :duration ( note-duration ( car notes-list ) )
                            :octave ( note-octave ( car notes-list ) )
                            :str-representation ( generate-str-representation new-pitch ( note-duration ( car notes-list ) ) ( note-octave ( car notes-list ) ) )
                        )
                    )
                    ( cons new-note ( create-harmonization ( cdr notes-list ) ) )
                )

            )
        
        )
    )
)



; Method for rearranging the elements of an input list
( defmethod list-permutation ( ( li list ) )
    (cond
        (( = 0 ( length li ) )
            '()
        )
        (t
            ( setf removed-element ( select-random-arr-element li ) )
            ( cons removed-element ( list-permutation ( remove removed-element li ) ) )
        )
    )
)

; Method to generate a random bassline based on the constraints of the system
( defmethod generate-bassline ()
    ( setf *constraint-arr* *bassline-durations* )
    ( setf dlist ( generate-durations '() ) )
    ( setf plist ( generate-pitches dlist ) )
    ( generate-notes dlist plist '() ( select-random-arr-element *bassline-octave* ) )
)


; Method to take pitches/durations/octaves, instantiate as note objs, and throw into list of notes
( defmethod generate-notes ( ( duration-list list ) ( pitch-list list ) (note-list list) octave )
    (cond
        (( = 0 ( length duration-list ) )
            
            note-list 
        )
        (t
            ( setf current-duration ( get-duration-representation ( car duration-list ) ) )
            ( setf current-pitch ( prin1-to-string ( car pitch-list ) ) )
            ( setf octave-rep ( get-octave-representation octave ) )
            ( setf str-rep ( concatenate 'string current-pitch current-duration octave-rep ))
            ( setf current-note 
                ( make-instance 'note 
                    :pitch ( car pitch-list )
                    :duration ( car duration-list )
                    :octave octave
                    :str-representation str-rep
                )
            )
            ( setf note-list ( append (list current-note) note-list  ) )
            ( generate-notes ( cdr duration-list ) ( cdr pitch-list ) note-list octave )
        )
    )
)


; Method to get the duration of a note as represented in EasyABC
( defmethod get-duration-representation ( duration )
    ( setf duration-assoc ( pairlis '(4 2 1 0.5 0.25 ) '("4" "2" "" "/2" "/4") ))
    ( cdr ( assoc duration duration-assoc ) )
)

; Method to get the octave of a note as represented in EasyABC
( defmethod get-octave-representation ( octave )
    (cond
        (( = octave 1 )
            "\,"
        )
        (( = octave 2 )
            ""
        )
        (( = octave 3 )
            "\'"
        )
    )
)

; Method that generates a list of pitches based on the
; length of duration-list
( defmethod generate-pitches ( ( duration-list list ) )
    ( cond 
        (( = ( length duration-list) 1 )
            ( list ( select-random-arr-element *CMAJOR* ) )
        )
        (t
            ( cons ( select-random-arr-element *CMAJOR* ) ( generate-pitches ( cdr duration-list ) ) )
        )
    )

)

; Method that generates a list of durations based on 
; the *beat-total*
( defmethod generate-durations ( ( duration-list list ) )
    ( cond 
        (( > ( sum duration-list ) *beat-total* )
            ( generate-durations '() )
        )
        (( = ( sum duration-list ) *beat-total* )
            duration-list
        )
        (t
            ( setf duration-list ( append duration-list ( list ( select-random-arr-element *constraint-arr* ) ) ) )
            ( generate-durations duration-list )
        )
    )
)

; Help Method that sums the elements of a list together -- used for duration calculation
( defmethod sum ( ( li list ) )
    (cond
        (( = ( length li ) 0 )
            0
        )
        (t
            ( + ( car li ) ( sum ( cdr li ) ) )
        )
    )
)

; Helper Method that selects a random element from a list
( defmethod select-random-arr-element ( ( li list ) )
    ( nth ( random ( length li ) ) li )
)

; Class for note representation
( defclass note ()
    (
        ( pitch :accessor note-pitch :initarg :pitch )
        ( duration :accessor note-duration :initarg :duration )
        ( octave :accessor note-octave :initarg :octave )
        ( str-representation :accessor note-str-representation :initarg :str-representation )
    )
)

; Method to generate the str-representation of a note
; based on pitch, duration, and octave
( defmethod generate-str-representation ( p d o )
    ( setf current-duration ( get-duration-representation d ) )
    ( setf current-pitch ( prin1-to-string p ) )
    ( setf octave-rep ( get-octave-representation o ) )
    ( concatenate 'string current-pitch current-duration octave-rep )
            
)

; Method to display the list of notes nicely
( defmethod display-notes-list (( notes-list list )) 
    ( mapcar #'note-str-representation notes-list )
)


; Knowledge-Base for Constraint System
( setf *CMAJOR* '( C D E F G A B ) )
( setf *melody-durations* '( 2 1 0.5 ) ) 
( setf *bassline-durations* '( 4 2 1 ) )
( setf *melody-octaves* '( 2 3 ) )
( setf *bassline-octave* '( 1 ) )

; Method to demo all three melody generation
( defmethod demo--generate-melodies ()
    ;Iteration 1
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
    (terpri)
    ;Iteration 2
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
    (terpri)
    ;Iteration 3
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
)

; Class for music representation
; - includes the list of notes for melody1, the list of notes for melody2,
;   the list of notes for melody3, and the string representation of all
;   melodies that can be copied/pasted to EasyABC to play the whole sample
( defclass music ()
    (
        ( melody1 :accessor music-melody1 :initarg :melody1 )
        ( melody2 :accessor music-melody2 :initarg :melody2 )
        ( melody3 :accessor music-melody3 :initarg :melody3 )
        ( str-representation :accessor music-str-representation :initarg :str-representation )
        ( rank :accessor music-rank :initarg :rank )
        ( num :accessor music-num :initarg :num )
    )
)

; Method to generate a music sample given an individual number. Used for the initial
; population creation.
( defmethod generate-music-sample ( i-num )
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )

    ( make-instance 'music
        :melody1 melody1-notes
        :melody2 melody2-notes
        :melody3 melody3-notes
        :str-representation ""
        :rank 0
        :num i-num
    )
)

; Method that displays melody1 of a music sample
( defmethod display-melody1 ( (m music) )
    ( format t "~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
)

; Method that displays melody2 of a music sample
( defmethod display-melody2 ( (m music) )
    ( format t "~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
)

; Method that displays melody3 of a music sample
( defmethod display-melody3 ( (m music) )
    ( format t "~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays melody1 and melody2 of a music sample
( defmethod display-melody1&2 ( (m music) )
    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
)

; Method that displays melody1 and melody3 of a music sample
( defmethod display-melody1&3 ( ( m music ) )
    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays melody2 and melody3 of a music sample
( defmethod display-melody2&3 ( ( m music ) )
    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
    ( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays all melodies of a music sample
( defmethod display-all-melodies ( ( m music ) )
    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
    ( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
    ( terpri )
)

; Method that demos the display methods of the music class
( defmethod demo--music-class ()
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            :melody3 melody3-notes
            :str-representation ""
            :rank 0
            :num 1
        )
    )

    ( print "----------------DISPLAY MELODY1 TEST----------------")
    (terpri)
    ( display-melody1 sample )
    (terpri)
    ( print "----------------DISPLAY MELODY2 TEST----------------")
    (terpri)
    ( display-melody2 sample )
    (terpri)
    ( print "----------------DISPLAY MELODY3 TEST----------------")
    (terpri)
    ( display-melody3 sample )
    (terpri)
    ( print "----------------DISPLAY MELODIES 1&2 TEST----------------")
    (terpri)
    ( display-melody1&2 sample )
    (terpri)
    ( print "----------------DISPLAY MELODIES 1&3 TEST----------------")
    (terpri)
    ( display-melody1&3 sample )
    (terpri)
    ( print "----------------DISPLAY MELODIES 2&3 TEST----------------")
    (terpri)
    ( display-melody2&3 sample )
    (terpri)
    ( print "----------------DISPLAY ALL MELODIES TEST----------------")
    (terpri)
    ( display-all-melodies sample )
)

;Global variable for testing "even" mutations vs. pitch + duration mutation
( setf *even-mutation* NIL )
; Method for mutating a music sample -- each melody is mutated

( defmethod mutation ( ( m music ) )
    ( setf melody1-list ( music-melody1 m ) )
    ( setf melody2-list ( music-melody2 m ) )
    ( setf melody3-list ( music-melody3 m ) )

    ( mutate-note-list melody1-list *CMAJOR* *melody-durations* )
    ( mutate-note-list melody2-list *CMAJOR* *melody-durations* )
    ( mutate-note-list melody3-list *CMAJOR* *bassline-durations* )

)

; Helper method for mutating an individual notes list -- the mutation 
; changes the pitch and duration of a single note
( defmethod mutate-note-list ( ( notes-list list ) ( pitch-list list ) ( duration-list list ) )
    ( setf note-to-change ( nth ( random ( length notes-list ) ) notes-list ) )
    ( setf new-note-pitch ( nth ( random ( length pitch-list ) ) pitch-list ) )
    ( setf new-note-duration ( nth ( random ( length duration-list ) ) duration-list ) )
    ( setf new-note-str ( generate-str-representation new-note-pitch new-note-duration ( note-octave note-to-change ) ) )

    ; if the same note is generated, recursively call on the method again
    (cond 
        ( ( and ( equal new-note-pitch ( note-pitch note-to-change ) )
                ( equal new-note-duration ( note-pitch note-to-change ) ) ) 
            ( mutate-note-list notes-list pitch-list duration-list )
        )
        (( and *even-mutation* ( equal new-note-pitch ( note-pitch note-to-change ) ) )
            ( mutate-note-list notes-list pitch-list duration-list )
        )
    )

    (cond
        (( not *even-mutation*)
            ( setf ( note-duration note-to-change ) new-note-duration )
        )
    )

    ( setf ( note-pitch note-to-change ) new-note-pitch )
    ( setf ( note-str-representation note-to-change ) new-note-str )

)

; Method for demoing mutation on a music sample
;   -Mutation is defined as changing the pitch and duration of a note in each of the melodies of
;   a music sample
;   -This method generates a music sample and performs mutation 4 times.
( defmethod demo--mutation ()
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            :melody3 melody3-notes
            :str-representation ""
            :rank 0
            :num 1
        )
    )
    
    ( format t "-------------------Trial 1-------------------~%")
    ( display-all-melodies sample )
    ( mutation sample )
    ( terpri )
    ( format t "-Mutation-~%" )
    ( display-all-melodies sample )
    ( terpri )
    ( format t "-------------------Trial 2-------------------~%")
    ( display-all-melodies sample )
    ( mutation sample )
    ( terpri )
    ( format t "-Mutation-~%" )
    ( display-all-melodies sample )
    ( terpri )
    ( format t "-------------------Trial 3-------------------~%")
    ( display-all-melodies sample )
    ( mutation sample )
    ( terpri )
    ( format t "-Mutation-~%" )
    ( display-all-melodies sample )
    ( terpri )
    ( format t "-------------------Trial 4-------------------~%")
    ( display-all-melodies sample )
    ( mutation sample )
    ( terpri )
    ( format t "-Mutation-~%" )
    ( display-all-melodies sample )
    ( terpri )

)

; Method to display a music sample in such a way that it can be copied and pasted
; directly into EasyABC notation
( defmethod easyabc-display ( ( sample music ) )
    ( format t "----------------Individual ~A----------------~%" ( music-num sample ) )
    ( format t "X:1~%")
    ( format t "T:Individual ~A~%" ( music-num sample ) )
    ( format t "C:Dystopian Tuesday~%" )
    ( format t "M:4/4~%" )
    ( format t "L:1/4~%" )
    ( format t "Q:1/4=120~%")
    ( format t "V:S clef=treble name=S~%" )
    ( format t "V:A clef=treble name=A~%" )
    ( format t "V:T clef=treble name=T~%" )
    ( format t "%%score [ ( S ) ( A ) ( T ) ]~%")
    ( format t "K:C~%" )
    ( format t "V:S~%" )
    ; set instrument for midi
    ( format t "%%MIDI program 0~%" )
    ( display-melody1 sample )
    ( format t "V:A~%" )
    ( display-melody2 sample )
    ( format t "V:T~%" )
    ( display-melody3 sample )
)

; Demo method to test the EasyABC display
( defmethod demo--display-individual ()
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            :melody3 melody3-notes
            :str-representation ""
            :rank 0
            :num 1
        )
    )

    ( easyabc-display sample )
    ( terpri )
    ( format t "---------------PERFORMING MUTATION---------------~%")
    ( mutation sample )
    ( easyabc-display sample )

)

; Global variable for the size of a population.
( defconstant *population-size* 100 )

; Global variable for the size of selection.
( defconstant *selection-size* 4 )

; Population Class -- consists of a list of music individuals and generation number.
( defclass population ()
    (
        ( individuals :accessor population-individuals :initarg :individuals )
        ( generation :accessor population-generation :initform 0 )
    )
)

; Calculates the size of a population.
( defmethod size ( ( p population ) )
    ( length ( population-indiviudals p ) )
)

; Displays the population in terms of salient information for the developer.
; (all melodies are not displayed -> instead individual #, 
; music object, and fitness are shown )
( defmethod display ( ( p population ) )
    ( terpri ) ( terpri )
    ( princ "Generation " )
    ( prin1 ( population-generation  p ) )
    ( princ " population ..." )
    ( terpri ) ( terpri )
    ( dolist ( i ( population-individuals p ) )

        ( prin1 ( music-num i ) )
        ( princ ( filler ( music-num i ) ) )

        ( prin1 i  )

        ( princ "  " )
        ( prin1 ( music-rank i ) )
        ( princ ( filler ( music-rank i ) ) )
         ( terpri )
        
        
    )
    ( terpri )
)

; Helper method to create space for individuals in population display method.
( defmethod filler ( ( n number ) )
    (cond
        ( ( < n 10 ) "     " )
        ( ( < n 100 ) "    " )
        ( ( < n 1000 ) "   " )
        ( ( < n 10000 ) "  " )
        ( ( < n 100000 ) " " )
    )

)

; Method to generate the initial population based on a population size.
( defmethod initial-population ( &aux individuals )
    ( setf individuals () )
    ( dotimes ( i *population-size* )
        ( push ( generate-music-sample ( + i 1 ) ) individuals )
    )
    ( make-instance 'population :individuals ( reverse individuals ) )
)

; Method to calculate the average fitness of a population.
( defmethod average ( ( p population ) &aux ( total 0 ) )
    ( setf indiv-list ( population-individuals p ) )
    ( setf total ( sum ( mapcar #'music-rank indiv-list ) ) )
    ( float ( / total *population-size* ) )
)

; Method to calculate and display the highest-ranked music individual in a list of
; music individuals.
( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual ) 
    ( setf max-individual ( max-val l 0 ) )
    ( setf max-value ( music-rank max-individual ) )
    max-individual 
)

; Method to calculate the maximum value given a list.
( defmethod max-val ( ( l list ) current-max )
    (cond
        (( null l )
            current-max
        )
        (( or ( equal current-max 0 ) ( > ( music-rank ( car l ) ) ( music-rank current-max ) ) )
            ( max-val ( cdr l ) ( car l ) )
        )
        (t
            ( max-val ( cdr l ) current-max )
        )
    )
)


; Demo to generate an initial population.
( defmethod population-demo ( &aux p )
    ( setf p ( initial-population ) )
    ( display p )
    ( format t "Average fitness = ~A~%~%" ( average p ) )
)

; Global variable that sets the number of individuals selected from a population.
( defconstant *selection-size* 4 )

; Method that selects the most-fit-individual from a list of candidates.
( defmethod select-individual ( ( p population ) 
    &aux i candidates rn )
    ( setf candidates ( select-individuals p ) )
    ( setf mfi ( most-fit-individual candidates ) )
    mfi    
)

; Method that randomly selects a number of individuals from a population 
; using *selection-size*.
( defmethod select-individuals ( ( p population )
    &aux individuals candidates rn )
    ( setf individuals ( population-individuals p ) )
    ( setf candidates () )
    ( dotimes ( i *selection-size* )
        ( setf rn ( random *population-size* ) )
        ( push ( nth rn individuals ) candidates )
    )
    candidates
)

; Method that handles the user-facing interactive selection process.
; -Lets the user rank the melody pairs and basslines of the number of 
;  music samples defined by *selection-size*.
; -Includes error-handling for user input.
( defmethod interactive-selection ( ( selected-samples list ) &aux melody1&2-rank bassline-rank)

    (cond
        (( null selected-samples )
            nil
        )
        (t
            ( setf current-sample ( car selected-samples ) )
            ( easyabc-display current-sample )
            ( format *query-io* "Melody 1 & 2 ranking (out of 10)? " )
            ( setf melody1&2-rank ( read-line *query-io* ) )

            ; ERROR-HANDLING for melody1&2-rank user input. Re-displays prompt if
            ;  1. Input is not a number.
            ;  2. Input is less than 0.
            ;  3. Input is greater than 10.
            ( loop while ( or ( not ( ignore-errors ( parse-integer melody1&2-rank ) ) ) 
                              ( < (parse-integer melody1&2-rank) 0 ) 
                              ( > (parse-integer melody1&2-rank) 10 ) )
              
               do ( format t "~%[ERROR] A ranking must be a natural number x such that -1 < x < 11.~%")
                    ( format *query-io* "Melody 1 & 2 ranking (out of 10)? " )
                    ( setf melody1&2-rank ( read-line *query-io* ) ) 
            )
            
            
            ( format *query-io* "Bassline ranking (out of 10)? " )
            ( setf bassline-rank ( read-line *query-io* ) )

            ; ERROR-HANDLING for bassline-rank user input. Re-displays prompt if
            ;  1. Input is not a number.
            ;  2. Input is less than 0.
            ;  3. Input is greater than 10.
            ( loop while ( or ( not ( ignore-errors ( parse-integer bassline-rank ) ) ) 
                              ( < ( parse-integer bassline-rank ) 0 ) 
                              ( > ( parse-integer bassline-rank ) 10 ) )
                
                do ( format t "~%[ERROR] A ranking must be a natural number x such that -1 < x < 11.~%")
                    ( format *query-io* "Bassline ranking (out of 10)? " )
                    ( setf bassline-rank ( read-line *query-io* ) )
            )

            ; Updates the ranks of the current sample.
            ( setf ( music-bassline-rank current-sample ) ( parse-integer bassline-rank ) )
            ( setf ( music-melodies-rank current-sample ) ( parse-integer melody1&2-rank ) )
            ( setf ( music-rank current-sample ) ( + ( parse-integer bassline-rank ) ( parse-integer melody1&2-rank ) ) ) 

            ( terpri )
            ( interactive-selection ( cdr selected-samples ) )

        )
    )

)


; for double-crossover add new selection methods based on top bassline and top melodies rankings :)

