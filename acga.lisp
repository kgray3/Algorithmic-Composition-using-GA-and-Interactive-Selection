; File: acga.lisp
; Hybridized constraint system and genetic algorithm for
; user-interactive algorithmic composition.

( setf *beat-total* 26 )
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
    )
)

; Method that displays melody1 of a music sample
( defmethod display-melody1 ( (m music) )
    ( format t "Melody 1: ~A~%" ( display-notes-list ( music-melody1 m ) ) )
)

; Method that displays melody2 of a music sample
( defmethod display-melody2 ( (m music) )
    ( format t "Melody 2: ~A~%" ( display-notes-list ( music-melody2 m ) ) )
)

; Method that displays melody3 of a music sample
( defmethod display-melody3 ( (m music) )
    ( format t "Melody 3: ~A~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays melody1 and melody2 of a music sample
( defmethod display-melody1&2 ( (m music) )
    ( format t "Melody 1: ~A~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list ( music-melody2 m ) ) )
)

; Method that displays melody1 and melody3 of a music sample
( defmethod display-melody1&3 ( ( m music ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays melody2 and melody3 of a music sample
( defmethod display-melody2&3 ( ( m music ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list ( music-melody2 m ) ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list ( music-melody3 m ) ) )
)

; Method that displays all melodies of a music sample
( defmethod display-all-melodies ( ( m music ) )
    ( format t "Melody 1: ~A~%" ( display-notes-list ( music-melody1 m ) ) )
    ( format t "Melody 2: ~A~%" ( display-notes-list ( music-melody2 m ) ) )
    ( format t "Melody 3: ~A~%" ( display-notes-list ( music-melody3 m ) ) )
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