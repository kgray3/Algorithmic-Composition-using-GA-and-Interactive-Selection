; File: acga.lisp
; Hybridized constraint system and genetic algorithm for
; user-interactive algorithmic composition.

( setf *beat-total* 24 )
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
    ( setf choice ( random 5 ) )
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
        (( = choice 3 )
            ( generate-bassline )
        )
        (( = choice 4 )
            ( create-octave-harmonization m1-notes )
        )
    )

)

; Method to create a harmonization of melody1 by using the same notes/duration
; in a different octave from melody1
( defmethod create-octave-harmonization ( ( notes-list list ) )
    (cond
        (( = 0 ( length notes-list ) )
            '()
        )
        (t
            ( setf new-octave ( car ( remove ( note-octave ( car notes-list ) ) *melody-octaves* ) ) )
            ( setf new-note
                ( make-instance 'note
                    :pitch ( note-pitch ( car notes-list ) )
                    :duration ( note-duration ( car notes-list ) )
                    :octave new-octave
                    :str-representation ( generate-str-representation 
                                            ( note-pitch ( car notes-list) )
                                            ( note-duration ( car notes-list ) )
                                            new-octave    
                                        )
                )
            )
            ( cons new-note ( create-octave-harmonization ( cdr notes-list ) ) )
        )
    )
)

; Method to demo the create-octave-harmonization method.
( defmethod demo--octave-harmonization ()
    ( setf melody1 ( generate-melody1 ) )
    ( setf melody2 ( create-octave-harmonization melody1 ) )

    ( setf m 
        ( make-instance 'music 
            :melody1 melody1
            :melody2 melody2
            :rank 0
            :melody1-rank 0
            :melody2-rank 0
            :num 2
        )
    
    )

    ( easyabc-display m )

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
    ( setf plist ( generate-pitches-stepwise dlist ( select-random-arr-element *CMAJOR* ) ) )
    ( generate-notes dlist plist '() ( select-random-arr-element *bassline-octave* ) )
)

; Method that generates a list of pitches based on the
; length of duration-list and stepwise movements
( defmethod generate-pitches-stepwise ( ( duration-list list ) previous 
    &aux stepwise-l ele previous-position )
    ( setf stepwise-l '( 0 1 1 2 ) )
    ( setf ele ( select-random-arr-element stepwise-l ) )
    ( setf previous-position ( position previous *CMAJOR* ) )
    ( cond 
        (( = ( length duration-list) 0 )
            '()
        )
        (( > ( + previous-position ele ) ( - ( length *CMAJOR* ) 1) )
            ( cons ( nth ( - previous-position ele ) *CMAJOR* ) 
                ( generate-pitches-stepwise ( cdr duration-list ) ( nth ( - previous-position ele ) *CMAJOR* ) ) )
        )
        (t
            ( cons ( nth ( + previous-position ele ) *CMAJOR* ) 
                ( generate-pitches-stepwise ( cdr duration-list ) ( nth ( + previous-position ele ) *CMAJOR* ) ) )
        )

    )

)

; Demo method for generating a bassline with stepwise motion.
( defmethod demo--stepwise-bassline ()
    ( setf melody1 ( generate-melody1 ) )
    ( setf melody2 ( generate-bassline ) )

    ( setf m 
        ( make-instance 'music 
            :melody1 melody1
            :melody2 melody2
            :rank 0
            :melody1-rank 0
            :melody2-rank 0
            :num 3
        )
    
    )

    ( easyabc-display m )
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
    ;( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ;( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
    (terpri)
    ;Iteration 2
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ;( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ;( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
    (terpri)
    ;Iteration 3
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ;( setf melody3-notes ( generate-bassline ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list melody1-notes ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list melody2-notes ) )
    ;( format t "Melody 3: ~A~%" ( display-notes-list melody3-notes ) )
)

; Class for music representation
; - includes the list of notes for melody1, the list of notes for melody2,
;   the list of notes for melody3, and the string representation of all
;   melodies that can be copied/pasted to EasyABC to play the whole sample
( defclass music ()
    (
        ( melody1 :accessor music-melody1 :initarg :melody1 )
        ( melody2 :accessor music-melody2 :initarg :melody2 )
        ;( melody3 :accessor music-melody3 :initarg :melody3 )
        ( rank :accessor music-rank :initarg :rank )
        ( melody1-rank :accessor music-melody1-rank :initarg :melody1-rank )
        ( melody2-rank :accessor music-melody2-rank :initarg :melody2-rank )
        ( num :accessor music-num :initarg :num )
    )
)

; Method to generate a music sample given an individual number. Used for the initial
; population creation.
( defmethod generate-music-sample ( i-num )
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ;( setf melody3-notes ( generate-bassline ) )

    ( make-instance 'music
        :melody1 melody1-notes
        :melody2 melody2-notes
        ;:melody3 melody3-notes
        :rank 0
        :melody1-rank 0
        :melody2-rank 0
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
;( defmethod display-melody3 ( (m music) )
;    ( format t "~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
;)

; Method that displays melody1 and melody2 of a music sample
;( defmethod display-melody1&2 ( (m music) )
;    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
;    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
;)

; Method that displays melody1 and melody3 of a music sample
;( defmethod display-melody1&3 ( ( m music ) )
;    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
;    ( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
;)

; Method that displays melody2 and melody3 of a music sample
;( defmethod display-melody2&3 ( ( m music ) )
;    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
;    ( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
;)

; Method that displays all melodies of a music sample
( defmethod display-all-melodies ( ( m music ) )
    ( format t "Melody 1: ~{~a~^ ~}~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 2: ~{~a~^ ~}~%" ( display-notes-list ( music-melody2 m ) ) )
    ;( format t "Melody 3: ~{~a~^ ~}~%" ( display-notes-list ( music-melody3 m ) ) )
    ( terpri )
)

; Method that demos the display methods of the music class
( defmethod demo--music-class ()
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ;( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            ;:melody3 melody3-notes
            :rank 0
            :melody1-rank 0
            :melody2-rank 0
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
    ;( print "----------------DISPLAY MELODY3 TEST----------------")
    ;(terpri)
    ;( display-melody3 sample )
    ;(terpri)
    ;( print "----------------DISPLAY MELODIES 1&2 TEST----------------")
    ;(terpri)
    ;( display-melody1&2 sample )
    ;(terpri)
    ;( print "----------------DISPLAY MELODIES 1&3 TEST----------------")
    ;(terpri)
    ;( display-melody1&3 sample )
    ;(terpri)
    ;( print "----------------DISPLAY MELODIES 2&3 TEST----------------")
    ;(terpri)
    ;( display-melody2&3 sample )
    ;(terpri)
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
    ;( setf melody3-list ( music-melody3 m ) )

    ( mutate-note-list melody1-list *CMAJOR* *melody-durations* )
    ( mutate-note-list melody2-list *CMAJOR* *constraint-arr* )
    ;( mutate-note-list melody3-list *CMAJOR* *bassline-durations* )

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
    ;( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            ;:melody3 melody3-notes
            :melody1-rank 0
            :melody2-rank 0
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
    ( format t "%%score [ ( S ) ( A ) ]~%")
    ( format t "K:C~%" )
    ( format t "V:S~%" )
    ; set instrument for midi
    ( format t "%%MIDI program 0~%" )
    ( display-melody1 sample )
    ( format t "V:A~%" )
    ( display-melody2 sample )
    ;( format t "V:T~%" )
    ;( format t "%%MIDI program 4~%" )
    ;( display-melody3 sample )
)

; Demo method to test the EasyABC display
( defmethod demo--display-individual ()
    ( setf melody1-notes ( generate-melody1 ) )
    ( setf melody2-notes ( generate-melody2 melody1-notes ) )
    ;( setf melody3-notes ( generate-bassline ) )

    ( setf sample 
        ( make-instance 'music
            :melody1 melody1-notes
            :melody2 melody2-notes
            ;:melody3 melody3-notes
            :rank 0
            :melody1-rank 0
            :melody2-rank 0
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
( setf *selection-size* 3 )

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



; Demo to generate an initial population.
( defmethod population-demo ( &aux p )
    ( setf p ( initial-population ) )
    ( display p )
    ( format t "Average fitness = ~A~%~%" ( average p ) )
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
( defmethod interactive-selection ( ( selected-samples list ) &aux melody1-rank melody2-rank)

    (cond
        (( null selected-samples )
            nil
        )
        (t
            ( setf current-sample ( car selected-samples ) )
            ( easyabc-display current-sample )
            ( format *query-io* "Melody 1 ranking (out of 10)? " )
            ( setf melody1-rank ( read-line *query-io* ) )

            ; ERROR-HANDLING for melody1&2-rank user input. Re-displays prompt if
            ;  1. Input is not a number.
            ;  2. Input is less than 0.
            ;  3. Input is greater than 10.
            ( loop while ( or ( not ( ignore-errors ( parse-integer melody1-rank ) ) ) 
                              ( < (parse-integer melody1-rank) 0 ) 
                              ( > (parse-integer melody1-rank) 10 ) )
              
               do ( format t "~%[ERROR] A ranking must be a natural number x such that -1 < x < 11.~%")
                    ( format *query-io* "Melody 1 ranking (out of 10)? " )
                    ( setf melody1-rank ( read-line *query-io* ) ) 
            )
            
            
            ( format *query-io* "Melody 2 ranking (out of 10)? " )
            ( setf melody2-rank ( read-line *query-io* ) )

            ; ERROR-HANDLING for bassline-rank user input. Re-displays prompt if
            ;  1. Input is not a number.
            ;  2. Input is less than 0.
            ;  3. Input is greater than 10.
            ( loop while ( or ( not ( ignore-errors ( parse-integer melody2-rank ) ) ) 
                              ( < ( parse-integer melody2-rank ) 0 ) 
                              ( > ( parse-integer melody2-rank ) 10 ) )
                
                do ( format t "~%[ERROR] A ranking must be a natural number x such that -1 < x < 11.~%")
                    ( format *query-io* "Melody 2 ranking (out of 10)? " )
                    ( setf melody2-rank ( read-line *query-io* ) )
            )

            ; Updates the ranks of the current sample.
            ( setf ( music-melody2-rank current-sample ) ( parse-integer melody2-rank ) )
            ( setf ( music-melody1-rank current-sample ) ( parse-integer melody1-rank ) )
            ( setf ( music-rank current-sample ) ( + ( parse-integer melody2-rank ) ( parse-integer melody1-rank ) ) ) 

            ( terpri )
            ( interactive-selection ( cdr selected-samples ) )

        )
    )

)


; Method to calculate and display the highest-ranked bassline rank in a list of
; music individuals.
( defmethod most-fit-melody2 ( ( selection list ) &aux max-value max-individual ) 
    ( setf max-individual ( max-val selection 0 #'music-melody2-rank ) )
    max-individual 
)

; Method to calculate and display the highest-ranked melodies rank in a list of
; music individuals.
( defmethod most-fit-melody1 ( ( selection list ) &aux max-value max-individual )
    ( setf max-individual ( max-val selection 0 #'music-melody1-rank ) )
    max-individual
)

; Method to calculate the maximum value given a list.
( defmethod max-val ( ( l list ) current-max func )
    (cond
        (( null l )
            current-max
        )
        (( or ( equal current-max 0 ) ( > ( funcall func ( car l ) ) ( funcall func current-max ) ) )
            ( max-val ( cdr l ) ( car l ) func )
        )
        (t
            ( max-val ( cdr l ) current-max func )
        )
    )
)

; Double-crossover method - takes four music individuals as input:
;    -m1 -> top melody1-rank music individual
;    -f1 -> second best melody1-rank music individual
;    -m2 -> top melody2-rank music individual
;    -f2 -> second best melody2-rank music individual
; Creates a new individual with a crossover of the two top melody1-rank music
; individuals and a crossover ot the two top melody2-rank music individuals
( defmethod double-crossover ( ( m1 music ) ( f1 music ) ( m2 music ) ( f2 music )
    &aux melody1-m1 melody2-m2 
         melody1-f1 melody2-f2 )
    
    ( setf melody1-m1 ( music-melody1 m1 ) )
    ( setf melody2-m2 ( music-melody2 m2 ) )

    ( setf melody1-f1 ( music-melody1 f1 ) )
    ( setf melody2-f2 ( music-melody2 f2 ) )

    ( setf m1-crossover ( melody-crossover melody1-m1 melody1-f1 ) )
    ( setf m2-crossover ( melody-crossover melody2-m2 melody2-f2 ) )

    ( setf new-melody2-rank ( + ( music-melody2-rank m2 ) ( music-melody2-rank f2 ) ) )
    ( setf new-melody1-rank ( + ( music-melody1-rank m1 ) ( music-melody1-rank f1 ) ) )
    ( setf new-music-rank ( + new-melody2-rank new-melody1-rank ) )

    ( make-instance 'music
        :melody1 m1-crossover
        :melody2 m2-crossover
        ;:melody3 m3-crossover
        :rank new-music-rank
        :melody2-rank new-melody2-rank
        :melody1-rank new-melody1-rank
        :num 0
    )


)

; Method that performs the crossover of a single melody by
; taking the first half of one melody notes list and putting
; it together with the latter half of the melody notes list based
; on a randomly generated duration within *beat-total*.
( defmethod melody-crossover ( ( m list ) ( f list ) &aux dpos )
    ( setf dpos ( + 1 ( random *beat-total* ) ) )

    (setf result ( append ( first-n m dpos ) ( rest-n f dpos ) ))

    (cond
        (( = ( sum ( mapcar #'note-duration result ) ) *beat-total* )
            result
        )
        (t
            ( melody-crossover m f )
        )
    )
    
    
)

; Method that returns the first half of a given list
; based on duration.
( defmethod first-n ( ( m list ) dpos )
    (cond
        (( <= dpos 0 )
            '()
        )
        (t
            ( cons ( car m ) ( first-n ( cdr m ) ( - dpos ( note-duration (car m ) ) ) ) )
        )
    )

)

; Method that returns the second half of a given list
; based on input duration.
( defmethod rest-n ( ( f list ) dpos )
    (cond
        (( <= dpos 0 )
            f
        )
        (t
            ( rest-n ( cdr f ) ( - dpos ( note-duration ( car f ) ) ) )
        )
    )
)

; Demo that showcases the most-fit-bassline and most-fit-melodies methods.
( defmethod demo--most-fit-individual ()
    ( setf popu ( initial-population ) )
    ( setf selection ( select-individuals popu ) )
    ( assign-random-ranks selection )

    ( format t "Selected melody1-rank...~A~%" ( mapcar #'music-melody1-rank selection ))
    
    ( format t "Most fit melody1-rank: ~A~%~%" ( music-melody1-rank ( most-fit-melody1 selection ) ) )

    ( format t "Selected melody2-rank...~A~%" ( mapcar #'music-melody2-rank selection ) )
    
    ( format t "Most fit melody2-rank: ~A~%" ( music-melody2-rank ( most-fit-melody2 selection ) ) )

)

; Demo method to show the crossover performed on a single melody.
( defmethod demo--crossover ()
    ( setf m ( generate-melody1 ) )
    ( setf f ( generate-melody1 ) )

    ( format t "------------------MELODY CROSSOVER TEST------------------~%")
    ( format t "Mother: ~A~%" ( display-notes-list m ) )
    ( format t "Father: ~A~%" ( display-notes-list f ) ) 

    ( format t "~% PERFORMING CROSSOVER... ~%" )
    ( setf new-melody ( melody-crossover m f ) )

    ( format t "Child: ~A~%" ( display-notes-list new-melody ) )
)

; Method for demoing double crossover.
( defmethod demo--double-crossover ()
    ( setf popu ( initial-population ) )
    ( setf selection ( select-individuals popu ) )
    ( assign-random-ranks selection )
    ( setf melody1-m ( most-fit-melody1 selection ) )
    ( setf melody1-f ( most-fit-melody2 ( remove melody1-m selection ) ) )
    ( format t "------------------MUSIC CROSSOVER TEST------------------~%")
    ( format t "Mother: ~%" )
    ( display-melody1 melody1-m ) 
    ( format t "Father: ~%" )
    ( display-melody1 melody1-f ) 
    


    ( setf melody2-m ( most-fit-melody2 selection ) )
    ( setf melody2-f ( most-fit-melody2 ( remove melody2-m selection ) ) )

    ( setf new-sample ( double-crossover melody1-m melody1-f melody2-m melody2-f ) )

    ( format t "Child: ~%" )
    ( display-melody1 new-sample )

    ( format t "Mother: ~%" )
    ( display-melody2 melody2-m )
    ( format t "Father: ~%" )
    ( display-melody2 melody2-f )
    ( format t "Child: ~%" )
    ( display-melody2 new-sample ) 

    ( easyabc-display new-sample )

)


; Demo helper method that randomly assigns ranks to a list of music individuals
; Meant to simulate interactive selection without me having to interact :)
( defmethod assign-random-ranks ( ( selection list ) &aux current-m )
    ( cond
        (( > ( length selection ) 0 )
            ( setf current-m ( car selection ) )
            ( setf ( music-melody2-rank current-m ) ( random 11 ) )
            ( setf ( music-melody1-rank current-m ) ( random 11 ) )
            ( setf ( music-rank current-m ) 
                ( + ( music-melody2-rank current-m ) ( music-melody1-rank current-m ) )
            )

            ( assign-random-ranks ( cdr selection ) )
        )
    )
)

; use sort of selection for finding most fit individuals by rank / get rid of car to find second

( defmethod demo--interactive-selection ()
    ( setf popu ( initial-population ) )
    ( setf selection ( select-individuals popu ) )
    ( interactive-selection selection )
)