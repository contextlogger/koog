(defun get-current-line ()
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun koog-filter-buffer/whole ()
  (let ((oldpoint (point)))
    (shell-command-on-region 
     (point-min) (point-max)
     (format "koog -i -o -f %s" buffer-file-name) nil t)
    (if (and (>= oldpoint (point-min))
	     (<= oldpoint (point-max)))
	(goto-char oldpoint))))

(defun koog-filter-buffer/whole/ia ()
  (interactive)
  (koog-filter-buffer/whole))

(defun koog-filter-buffer/point ()
  (let ((oldpoint (point)))
    (shell-command-on-region 
     (point-min) (point-max)
     (format "koog -i -o -f %s -l %d" buffer-file-name (get-current-line)) nil t)
    (if (and (>= oldpoint (point-min))
	     (<= oldpoint (point-max)))
	(goto-char oldpoint))))

(defun koog-filter-buffer/point/ia ()
  (interactive)
  (koog-filter-buffer/point))

(defun koog-filter-buffer/remove ()
  (let ((oldpoint (point)))
    (shell-command-on-region 
     (point-min) (point-max)
     (format "koog -r -i -o -f %s -l %d" buffer-file-name (get-current-line)) nil t)
    (if (and (>= oldpoint (point-min))
	     (<= oldpoint (point-max)))
	(goto-char oldpoint))))

(defun koog-filter-buffer/remove/ia ()
  (interactive)
  (koog-filter-buffer/remove))

(defun koog-insert-markers/ia ()
  (interactive)
  (insert "/***koog ")
  (save-excursion
    (insert " ***//***end***/")))

;; A keymap you might optionally use, with bindings similar to the
;; ones for Vim. The Vim prefix is "m", you choose the prefix for
;; these, e.g. with (define-key c-mode-map [(control o) (m)]
;; 'koog-map-prefix).
(defvar koog-map (make-keymap) 
  "Keymap for accessing Koog functions")
(fset 'koog-map-prefix koog-map)
(define-key koog-map "a" 'koog-filter-buffer/whole/ia)
(define-key koog-map "e" 'koog-filter-buffer/point/ia)
(define-key koog-map "i" 'koog-insert-markers/ia)
(define-key koog-map "r" 'koog-filter-buffer/remove/ia)

(provide 'koog)
