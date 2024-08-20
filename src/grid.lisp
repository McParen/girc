(in-package :de.anvi.girc)

(defclass grid-1d ()
  ((grid-pointer ; gp
    :initform      nil
    :type          (or null integer)
    :accessor      grid-pointer
    :documentation "Number of the currently selected item, nil if the list is empty.")

   (grid-length    ; gl
    :initarg       :grid-length
    :initform      nil
    :type          (or null integer)
    :documentation "")

   (scrolling-enabled-p
    :initarg       :enable-scrolling
    :initform      nil
    :type          boolean)

   (region-start   ; rp
    :initform      nil
    :type          (or null integer)
    :documentation "")

   (region-length  ; rl
    :initarg       :region-length
    :initform      nil
    :type          (or null integer)
    :documentation "")

   (cyclicp
    :initarg       :cyclic
    :initform      nil
    :accessor      cyclicp
    :type          boolean
    :documentation
    "If true, the first element is selected after the last.

For this to work, grid-length has to be set."))

  (:documentation "
Grid class that is used to keep track of the current item and visible
scrolling region of a linear display, for example a row or column.

While a 2D grid can move up, down, left, right and keep track of the
currentl visible rectangle, 1D grid can only move to previous and next
and keep track of the currently visible line, for example a tab bar
with 10 open tabs where only 6 can be displayed.

A 1D grid has a length gl and the current position (input pointer) gp.

The region is the visible part of the grid, has a start pointer rp and
a length rl.

            rp          rp+rl
            |-----|-----|

|-----|-----|-----|-----|-----|
0                 gp          gl
"))

(defmethod initialize-instance :after ((obj grid-1d) &key items)
  (with-slots (scrolling-enabled-p
               (p grid-pointer)
               (l grid-length)
               (rs region-start)
               (rl region-length)) obj
    ;; we start with one main buffer.
    (setf l 1
          p 0)))

(defun move-next (grid-1d)
  (with-slots (cyclicp
               scrolling-enabled-p
               (p grid-pointer)
               (l grid-length)
               (rs region-start)
               (rl region-length)) grid-1d
    (when (plusp l)
      (if scrolling-enabled-p
          (if cyclicp
              (progn
                (setf p (mod (1+ p) l))
                (when (>= p (+ rs rl))
                  (incf rs))
                (when (zerop p)
                  (setf rs 0)))
              (progn
                (when (< p (1- l))
                  (incf p))
                (when (>= p (+ rs rl))
                  (incf rs))))
          (if cyclicp
              (setf p (mod (1+ p) l))
              (when (< p (1- l))
                (incf p)))))))

(defun move-previous (grid-1d)
  (with-slots (cyclicp
               scrolling-enabled-p
               (p grid-pointer)
               (l grid-length)
               (rs region-start)
               (rl region-length)) grid-1d
    (when (plusp l)
      (if scrolling-enabled-p
          (if cyclicp
              (progn
                (setf p (mod (1- p) l))
                (cond ((< p rs)
                       (decf rs))
                      ((= p (1- l))
                       (setf rs (- l rl)))))
              (progn
                (when (> p 0)
                  (decf p))
                (when (< p rs)
                  (decf rs))))
          (if cyclicp
              (setf p (mod (1- p) l))
              (when (> p 0)
                (decf p)))))))

(defun grid-move-nth (n grid-1d)
  "Set the current item pointer to n."
  (with-slots (scrolling-enabled-p
               (p grid-pointer)
               (rs region-start)
               (rl region-length)) grid-1d
    (setf p n)
    (when scrolling-enabled-p
      (unless rs
        (setf rs 0))
      (setf rs (cond ((< n rs)
                      n)
                     ((>= n (+ rs rl))
                      (1+ (- n rl)))
                     (t
                      rs))))))

(defun grid-insert-nth (n grid-1d)
  "Update the grid after an item has been inserted at nth position."
  (with-slots (scrolling-enabled-p
               (p grid-pointer)
               (l grid-length)
               (rs region-start)
               (rl region-length)) grid-1d
    ;; Set the current item pointer to the newly inserted item.
    (setf p n)
    ;; Increase the length of the grid.
    (setf l (1+ l))
    (when (> l rl)
      (setf scrolling-enabled-p t))
    (when scrolling-enabled-p
      (unless rs
        (setf rs 0))
      (setf rs (cond ((< n rs)
                      n)
                     ((>= n (+ rs rl))
                      (1+ (- n rl)))
                     (t
                      rs))))))

(defun grid-remove-nth (n grid-1d)
  "Update the grid after an item has been removed from nth position."
  (with-slots (scrolling-enabled-p
               (p grid-pointer)
               (l grid-length)
               (rs region-start)
               (rl region-length)) grid-1d
    ;; if new position is before the current pointer or at the last
    (when (or (< n p)
              (= n p (1- l)))
      (decf p))
    ;; decrease the length of the grid.
    (decf l)
    ;; disable scrolling if all of the grid is visible.
    (when (< l rl)
      (setf scrolling-enabled-p nil))
    ;; adjust region start
    (when scrolling-enabled-p
      (when (and (> rs 0)
                 (or (< n rs)
                     (and (>= n rs)
                          (>= (+ rs rl) l))))
        (decf rs)))))
