(in-package :peteai.nietzsche.trial)

(defun nietzsche-deliberate (input &key (return :plist))
  (peteai.nietzsche:nietzsche-deliberate input :return return))

(in-package :cl-user)

(defstruct player-state
  (round 0)
  (history '())
  (notes '()))

(defun init-player-state ()
  "Initializer (do NOT name this make-player-state)."
  (make-player-state :round 0 :history '() :notes '()))

(defparameter *nietzsche-modes*
  '((:genealogy
     "Why does this belief feel necessary to you?"
     "What problem disappears if this belief is true?"
     "When did you inherit or adopt this idea?")
    (:pressure
     "What fear does this belief protect you from?"
     "What happens if you must live without this idea?"
     "Who gains power if this belief is unquestioned?")
    (:reframe
     "Rewrite this without moral language."
     "State it as a goal with tradeoffs instead of a rule."
     "Describe it as a preference, not a command.")
    (:inversion
     "What if the opposite were life-enhancing?"
     "Who might flourish under a reversed value?"
     "What strength is currently being excluded?")
    (:creation
     "What could be built instead of criticized?"
     "What does this belief make possible?"
     "If this were art, what kind would it be?")))
     
(defun move-instructions (move)
  (case (char move 0)
    (#\A "Move A: Rewrite as a description. No should/must. No good/bad. Just what IS.")
    (#\B "Move B: Use this template: 'If we want X, we accept Y cost.'")
    (#\C "Move C: State the opposite value and defend it in one sentence.")
    (#\D "Move D: Define 2–3 key terms in your statement.")
    (t   "Freeform: respond however you want.")))


(defun pick-prompts (n)
  (loop repeat n
        collect
        (destructuring-bind (mode . prompts)
            (nth (random (length *nietzsche-modes*)) *nietzsche-modes*)
          (declare (ignore mode))
          (nth (random (length prompts)) prompts))))

(defun play-nietzsche-round (input &key (prompt-count 3))
  "Run a single trial round. INPUT is required."
  (let* ((analysis (nietzsche-deliberate input :return :plist))
         (flags (getf analysis :flags))
         (questions (getf analysis :questions))
         (notes (getf analysis :notes))
         (prompts (pick-prompts prompt-count)))

    (format t "~%NIETZSCHE TRIAL INITIATED~%")
    (format t "~%Original Statement:~%~a~%" input)

    (when flags
      (format t "~%Detected Pressure Points:~%")
      (dolist (f flags) (format t "  - ~a~%" f)))

    (when notes
      (format t "~%Notes:~%")
      (dolist (n notes) (format t "  * ~a~%" n)))

    (format t "~%Genealogical Questions:~%")
    (dolist (q questions) (format t "  • ~a~%" q))

    (format t "~%Trial Prompts:~%")
    (loop for p in prompts
          for i from 1
          do (format t "  [~a] ~a~%" i p))

    (format t "~%Your response (freeform):~%")
    (let ((response (read-line)))
      (list :input input :analysis analysis :prompts prompts :response response))))

(defun run-nietzsche-trial (&key (rounds 3))
  "Multi-round REPL loop."
  (let ((state (init-player-state)))
    (dotimes (i rounds state)
      (incf (player-state-round state))
      (format t "~%==============================~%")
      (format t "NIETZSCHE TRIAL — ROUND ~a~%" (1+ i))
      (format t "==============================~%")
      (format t "~%Enter a belief/claim/statement:~%")

      (let* ((input (read-line))
             (result (play-nietzsche-round input)))
        (push result (player-state-history state))

        (format t "~%Reflection (optional): What shifted, if anything?~%")
        (push (read-line) (player-state-notes state))))

    (format t "~%TRIAL COMPLETE. Rounds: ~a~%" (player-state-round state))
    state))

