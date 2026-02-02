;;;; rogue-lines.lisp — terminal generator, writes SVG, no browser required
;;;; SBCL/Common Lisp, no external deps.

(defpackage :rogue-lines
  (:use :cl)
   (:export #:cmd-lines #:gen-lines->svg #:gen-overlay->svg))
(in-package :rogue-lines)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype function gen-lines->svg))
  (declaim (ftype function mirror-path)))




;;; ---------- helpers ----------
(defun randi (a b) (+ a (random (1+ (- b a))))) ; inclusive
(defun randf (a b) (+ a (* (random 1.0) (- b a))))
(defun clamp (v lo hi) (min hi (max lo v)))

;;; ---------- generators ----------
(defun gen-random-walk (&key (w 980) (h 620) (n 12) (steps 180) (step-len 6) (jitter 2))
  (loop repeat n collect
    (let* ((x (randi 50 (- w 50)))
           (y (randi 50 (- h 50)))
           (ang (randf 0.0 (* 2 pi)))
           (path (list (list x y))))
      (loop repeat steps do
        (setf ang (+ ang (randf -0.45 0.45)))
        (let* ((dx (+ (* (cos ang) step-len) (randf (- jitter) jitter)))
               (dy (+ (* (sin ang) step-len) (randf (- jitter) jitter))))
          (setf x (round (clamp (+ x dx) 5 (- w 5))))
          (setf y (round (clamp (+ y dy) 5 (- h 5))))
          (push (list x y) path)))
      (nreverse path))))

(defun gen-scribble (&key (w 980) (h 620) (n 12) (jitter 2))
  (loop repeat n collect
    (let* ((x (randi 40 (- w 40)))
           (y (randi 40 (- h 40)))
           (segments (randi 6 26))
           (path (list (list x y))))
      (loop repeat segments do
        (incf x (+ (randi -90 90) (randi (- jitter) jitter)))
        (incf y (+ (randi -90 90) (randi (- jitter) jitter)))
        (setf x (clamp x 5 (- w 5)))
        (setf y (clamp y 5 (- h 5)))
        (push (list (round x) (round y)) path))
      (nreverse path))))

(defun gen-arcs (&key (w 980) (h 620) (n 12) (jitter 2))
  (loop repeat n collect
    (let* ((cx (randi 80 (- w 80)))
           (cy (randi 80 (- h 80)))
           (r  (randi 30 240))
           (start (randi 0 359))
           (extent (randi 30 320))
           (steps (randi 25 120))
           (path '()))
      (loop for i from 0 to steps do
        (let* ((theta (+ (* (/ i steps) (* extent (/ pi 180.0)))
                         (* start (/ pi 180.0))))
               (x (+ cx (* (cos theta) r) (randf (- jitter) jitter)))
               (y (+ cy (* (sin theta) r) (randf (- jitter) jitter))))
          (push (list (round (clamp x 5 (- w 5)))
                      (round (clamp y 5 (- h 5))))
                path)))
      (nreverse path))))

(defun gen-burst (&key (w 980) (h 620) (n 12) (jitter 2))
  ;; returns many 2-point paths (rays)
  (loop repeat n append
    (let* ((cx (randi 80 (- w 80)))
           (cy (randi 80 (- h 80)))
           (rays (randi 8 40))
           (base (randf 0.0 (* 2 pi))))
      (loop for r-i from 0 below rays collect
        (let* ((ang (+ base (* (/ r-i rays) (* 2 pi)) (randf -0.18 0.18)))
               (len (randi 40 320))
               (x1 (+ cx (* (cos ang) len) (randf (- jitter) jitter)))
               (y1 (+ cy (* (sin ang) len) (randf (- jitter) jitter))))
          (list (list cx cy)
                (list (round (clamp x1 5 (- w 5)))
                      (round (clamp y1 5 (- h 5))))))))))

(defun gen-trig-poly (&key (w 980) (h 620)
                           (steps 1400)
                           (a 0.0009)
                           (amp 140.0)
                           (freq 0.025)
                           (phase 0.0)
                           (dx 0.7)
                           (jitter 0))
  "Polynomial + trigonometric parametric curve.
Returns a list of one path: (((x y) ...))."
  (let ((cx (/ w 2.0))
        (cy (/ h 2.0))
        (path '()))
    (loop for i from 0 below steps do
      (let* ((theta (+ (* freq i) phase))
             (x (+ cx (* dx (- i (/ steps 2.0)))
                   (if (> jitter 0) (randf (- jitter) jitter) 0.0)))
             (y (+ cy
                   (* a (expt (- i (/ steps 2.0)) 2))
                   (* amp (sin theta))
                   (if (> jitter 0) (randf (- jitter) jitter) 0.0))))
        (push (list (round (clamp x 5 (- w 5)))
                    (round (clamp y 5 (- h 5))))
              path)))
    (list (nreverse path))))
    
(defun gen-trig-poly-field (&key (w 980) (h 620)
                                 (voices 18)
                                 (steps 1400)
                                 (a 0.0009)
                                 (amp 140.0)
                                 (freq 0.025)
                                 (phase 0.0)
                                 (dx 0.7)
                                 (jitter 0)
                                 ;; variation ranges
                                 (amp-spread 60.0)
                                 (freq-spread 0.015)
                                 (phase-spread 6.2831853) ; ~2*pi
                                 (a-spread 0.0012)
                                 (dx-spread 0.6))
  "Generate many trig-poly curves (a field). Returns a list of paths."
  (loop repeat voices collect
    (let* ((aa (+ a   (randf (- a-spread)   a-spread)))
           (bb (+ amp (randf (- amp-spread) amp-spread)))
           (ff (+ freq (randf (- freq-spread) freq-spread)))
           (pp (+ phase (randf 0.0 phase-spread)))
           (dd (+ dx  (randf (- dx-spread)  dx-spread))))
      (first (gen-trig-poly :w w :h h
                            :steps steps
                            :a aa :amp bb :freq ff :phase pp :dx dd
                            :jitter jitter)))))
                            
(defun gen-mirror-overlay (&key (w 980) (h 620) (base-mode :walk)
                                (n 10) (steps 220) (step-len 7) (jitter 2))
  "Generate base paths and add mirrored copies."
  (let ((paths
          (ecase base-mode
            (:walk (gen-random-walk :w w :h h :n n :steps steps :step-len step-len :jitter jitter))
            (:scribble (gen-scribble :w w :h h :n n :jitter jitter))
            (:arcs (gen-arcs :w w :h h :n n :jitter jitter))
            (:burst (gen-burst :w w :h h :n n :jitter jitter))
            (:trig-poly (gen-trig-poly :w w :h h :steps steps :jitter jitter))
            (:polyfield (gen-trig-poly-field :w w :h h :voices n :steps steps :jitter jitter)))))
    (append
     paths
     (loop for p in paths append
       (list (mirror-path p :w w :h h :mode :x  :dx (randi -6 6) :dy (randi -3 3))
             (mirror-path p :w w :h h :mode :y  :dx (randi -3 3) :dy (randi -6 6)))))))
             
(defun mirror-path (path &key (w 980) (h 620) (mode :x) (dx 0) (dy 0))
  "Mirror a PATH across :x (vertical axis), :y (horizontal), or :xy (both)."
  (labels ((m (pt)
             (destructuring-bind (x y) pt
               (let ((mx (case mode
                           (:x  (- w x))
                           (:y  x)
                           (:xy (- w x))
                           (t x)))
                     (my (case mode
                           (:x  y)
                           (:y  (- h y))
                           (:xy (- h y))
                           (t y))))
                 (list (round (clamp (+ mx dx) 5 (- w 5)))
                       (round (clamp (+ my dy) 5 (- h 5))))))))
    (mapcar #'m path)))
             
(defun gen-crosshatch (&key (w 980) (h 620) (gap 30) (slant 0.6))
  "Returns a list of 2-point paths forming diagonal hatch lines."
  (let ((paths '()))
    ;; diagonal down-right
    (loop for y from (- h) to (* 2 h) by gap do
      (let* ((x0 0)
             (y0 y)
             (x1 w)
             (y1 (+ y (* slant w))))
        (push (list (list (round (clamp x0 0 w)) (round (clamp y0 0 h)))
                    (list (round (clamp x1 0 w)) (round (clamp y1 0 h))))
              paths)))
    ;; diagonal down-left
    (loop for y from (- h) to (* 2 h) by gap do
      (let* ((x0 w)
             (y0 y)
             (x1 0)
             (y1 (+ y (* slant w))))
        (push (list (list (round (clamp x0 0 w)) (round (clamp y0 0 h)))
                    (list (round (clamp x1 0 w)) (round (clamp y1 0 h))))
              paths)))
    (nreverse paths)))

             
(defun gen-trochoid (&key (w 980) (h 620) (steps 1800)
                          (r 120.0) (s 45.0) (d 95.0)
                          (scale 1.0) (phase 0.0))
  "Hypotrochoid-ish curve. Returns one path."
  (let ((cx (/ w 2.0)) (cy (/ h 2.0)) (path '()))
    (loop for i from 0 below steps do
      (let* ((theta (+ phase (* i 0.02)))
             (x (+ cx (* scale (+ (* (- r s) (cos theta))
                                  (* d (cos (* (/ (- r s) s) theta)))))))
             (y (+ cy (* scale (+ (* (- r s) (sin theta))
                                  (* (- d) (sin (* (/ (- r s) s) theta))))))))
        (push (list (round (clamp x 5 (- w 5)))
                    (round (clamp y 5 (- h 5))))
              path)))
    (list (nreverse path))))
    




;;; ---------- svg writer ----------

(defun %svg-path-d (points)
  (with-output-to-string (s)
    (destructuring-bind (x0 y0) (first points)
      (format s "M ~d ~d" x0 y0))
    (dolist (pt (rest points))
      (destructuring-bind (x y) pt
        (format s " L ~d ~d" x y)))))

(defun %ensure-paths (paths mode)
  (unless (and (listp paths) paths)
    (error "Mode ~s produced no paths. Did you forget to wire it into ECASE or return a list of paths?" mode))
  paths)



    
(defun gen-overlay->svg (outfile layers &key (w 980) (h 620) (bg "white"))
  "LAYERS is a list of plist layers.
Each layer can be either:
  - generator-based: (:mode :walk ... :n 12 :steps 200 ...)
  - path-based:      (:paths <list-of-paths> ...)

Example:
  (list
    '(:mode :polyfield :stroke 1 :opacity 0.22 :n 40 :steps 1300 :jitter 2)
    (list :paths (gen-crosshatch :w 980 :h 620 :gap 26 :slant 0.55)
          :stroke 1 :opacity 0.10))"
  (with-open-file (out outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "<?xml version='1.0' encoding='UTF-8'?>~%")
    (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~d' height='~d' viewBox='0 0 ~d ~d'>~%" w h w h)
    (format out "  <rect width='100%' height='100%' fill='~a'/>~%" bg)

    (loop for layer in layers
          for idx from 1 do
            (destructuring-bind (&key
                                  (mode :burst)
                                  paths
                                  (stroke 3) (opacity 0.35) (ink "black")
                                  (n 12) (steps 180) (step-len 6) (jitter 2)
                                  (a 0.0009) (amp 140.0) (freq 0.025) (phase 0.0) (dx 0.7)
                                  (gap 30) (slant 0.6)
                                  (r 120.0) (s 45.0) (d 95.0) (scale 1.0)
                                  &allow-other-keys)
                layer
              (let* ((the-paths
                      (or paths
                          (ecase mode
                            (:walk (gen-random-walk :w w :h h :n n :steps steps :step-len step-len :jitter jitter))
                            (:scribble (gen-scribble :w w :h h :n n :jitter jitter))
                            (:arcs (gen-arcs :w w :h h :n n :jitter jitter))
                            (:burst (gen-burst :w w :h h :n n :jitter jitter))
                            (:trig-poly (gen-trig-poly :w w :h h :steps steps :a a :amp amp :freq freq :phase phase :dx dx :jitter jitter))
                            (:polyfield (gen-trig-poly-field :w w :h h :voices n :steps steps
                                                             :a a :amp amp :freq freq :phase phase :dx dx :jitter jitter))
                            (:hatch (gen-crosshatch :w w :h h :gap gap :slant slant))
                            (:trochoid (gen-trochoid :w w :h h :steps steps :r r :s s :d d :scale scale :phase phase))))))

                (format out "  <g id='layer-~d' opacity='~f'>~%" idx opacity)
                (dolist (p the-paths)
                  (format out
                          "    <path d='~a' fill='none' stroke='~a' stroke-width='~d' stroke-linecap='round' stroke-linejoin='round'/>~%"
                          (%svg-path-d p) ink stroke))
                (format out "  </g>~%"))))

    (format out "</svg>~%"))
  outfile)

(in-package :rogue-lines)

(defun gen-lines->svg (outfile &key
                               (mode :burst)
                               (w 980) (h 620)
                               (stroke 3)
                               (n 12) (steps 180) (step-len 6) (jitter 2)
                               (bg "white") (ink "black")
                               (opacity 1.0))
  (let ((paths
          (ecase mode
            (:walk (gen-random-walk :w w :h h :n n :steps steps :step-len step-len :jitter jitter))
            (:scribble (gen-scribble :w w :h h :n n :jitter jitter))
            (:arcs (gen-arcs :w w :h h :n n :jitter jitter))
            (:burst (gen-burst :w w :h h :n n :jitter jitter))
            (:trig-poly (gen-trig-poly :w w :h h :steps steps :jitter jitter))
            (:polyfield (gen-trig-poly-field :w w :h h :voices n :steps steps :jitter jitter))
            (:trochoid (gen-trochoid :w w :h h :steps steps)))))
    (with-open-file (out outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "<?xml version='1.0' encoding='UTF-8'?>~%")
      (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~d' height='~d' viewBox='0 0 ~d ~d'>~%" w h w h)
      (format out "  <rect width='100%' height='100%' fill='~a'/>~%" bg)
      (dolist (p paths)
        (format out "  <path d='~a' fill='none' stroke='~a' stroke-width='~d' stroke-linecap='round' stroke-linejoin='round' opacity='~f'/>~%"
                (%svg-path-d p) ink stroke opacity))
      (format out "</svg>~%"))
    outfile))


;;; ---------- cli ----------
(defun %normalize-mode (raw)
  (let* ((s (string-downcase (string-trim '(#\Space #\Tab) raw))))
    (cond
      ((or (string= s "") (string= s ":burst") (string= s "burst")) :burst)
      ((or (string= s ":walk") (string= s "walk")) :walk)
      ((or (string= s ":scribble") (string= s "scribble")) :scribble)
      ((or (string= s ":arcs") (string= s "arcs")) :arcs)
      ((or (string= s ":trig-poly") (string= s "trig-poly")) :trig-poly)
      ((or (string= s ":polyfield") (string= s "polyfield")) :polyfield)
      ((or (string= s ":hatch") (string= s "hatch")) :hatch)
      ((or (string= s ":trochoid") (string= s "trochoid")) :trochoid)
      (t :burst))))

(defun cmd-lines ()
  "Terminal command: prompt, generate, write SVG."
  (format t "~&Rogue Line Space (SVG)~%")
  (format t "Mode [walk scribble arcs burst trig-poly polyfield trochoid] (default burst): ")
  (finish-output)
  (let* ((raw (read-line))
         (mode (%normalize-mode raw)))
    (format t "Stroke width (default 4): ")
    (finish-output)
    (let* ((stroke-raw (string-trim '(#\Space #\Tab) (read-line)))
           (stroke (if (zerop (length stroke-raw)) 4 (parse-integer stroke-raw :junk-allowed t)))
           (outfile (format nil "rogue-lines-~(~a~)-~d.svg" mode (get-universal-time))))
      (gen-lines->svg outfile :mode mode :stroke (max 1 stroke)
                      :n 14 :steps 220 :step-len 7 :jitter 3 :opacity 0.95)
      (format t "~&Wrote: ~a~%" outfile)
      (format t "Open with: xdg-open ~a~%" outfile)
      outfile)))

