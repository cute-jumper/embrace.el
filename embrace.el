;;; embrace.el --- Add/Change/Delete pairs based on `expand-region'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((emacs "24.4") (expand-region "0.10.0") (cl-lib "0.5"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'expand-region)
(require 'cl-lib)

(cl-defstruct embrace-pair-struct
  key left right left-regexp right-regexp read-function)

(defvar embrace-evil-surround-key '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?b ?B ?t)
  "Keys that should be processed by `evil-surround'")
(make-variable-buffer-local 'embrace-evil-surround-key)

(defvar embrace-semantic-units-alist '((?w . er/mark-word)
                                       (?s . er/mark-symbol)
                                       (?d . er/mark-defun)
                                       (?P . er/mark-inside-pairs)
                                       (?p . er/mark-outside-pairs)
                                       (?Q . er/mark-inside-quotes)
                                       (?q . er/mark-outside-quotes)
                                       (?. . er/mark-sentence)
                                       (?h . er/mark-paragraph))
  "Key to mark function mapping.")
(make-variable-buffer-local 'embrace-semantic-units-alist)

(defvar embrace--pairs-list nil)

(defun embrace-add-pair (key left right)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :left left
                          :right right
                          :left-regexp (regexp-quote left)
                          :right-regexp (regexp-quote right)))))

(defun embrace-add-pair-regexp (key left-regexp right-regexp read-function)
  (assq-delete-all key embrace--pairs-list)
  (add-to-list 'embrace--pairs-list
               (cons key (make-embrace-pair-struct
                          :key key
                          :read-function read-function
                          :left-regexp left-regexp
                          :right-regexp right-regexp))))

(defun embrace--setup-defaults ()
  (dolist (pair '((?\( . ("(" . ")"))
                  (?\) . ("( " . " )"))
                  (?\{ . ("{" . "}"))
                  (?\} . ("{ " . " }"))
                  (?\[ . ("[" . "]"))
                  (?\] . ("[ " . " ]"))
                  (?> . ("<" . ">"))
                  (?\" . ("\"" . "\""))
                  (?\' . ("\'" . "\'"))))
    (embrace-add-pair (car pair) (cadr pair) (cddr pair)))
  (embrace-add-pair-regexp ?t "u]*?>" "u]*?>" 'embrace-with-tag)
  (embrace-add-pair-regexp ?f "\\(\\w\\|\\s_\\)+?(" ")" 'embrace-with-function))

(embrace--setup-defaults)
(make-variable-buffer-local 'embrace--pairs-list)

(defun embrace-with-tag ()
  (let* ((input (read-string "Tag: "))
         (match (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" (or tag "") (or rest ""))
          (format "</%s>" (or tag "")))))

(defun embrace-with-function ()
  (let ((fname (read-string "Function: ")))
    (cons (format "%s(" (or fname "")) ")")))

(defun embrace--get-region-overlay (open close)
  (cl-letf (((symbol-function 'message) (lambda (&rest args) nil)))
    (let ((expand-region-fast-keys-enabled nil))
      (save-excursion
        (when (looking-at open)
          (er/expand-region 1))
        (while (and (not (= (point) (point-min)))
                    (not (and (looking-at open)
                              (save-excursion
                                (goto-char (region-end))
                                (looking-back close)))))
          (er/expand-region 1))
        (when (not (and (looking-at open)
                        (save-excursion
                          (goto-char (region-end))
                          (looking-back close))))
          (setq mark-active nil))
        (when (use-region-p)
          (make-overlay (region-beginning) (region-end) nil nil t))))))

(defun embrace--insert (char overlay)
  (let* ((struct (assoc-default char embrace--pairs-list))
         open close)
    (if struct
        (if (functionp (embrace-pair-struct-read-function struct))
            (let ((pair (funcall (embrace-pair-struct-read-function struct))))
              (setq open (car pair))
              (setq close (cdr pair)))
          (setq open (embrace-pair-struct-left struct))
          (setq close (embrace-pair-struct-right struct)))
      (let ((char-str (char-to-string char)))
        (setq open char-str
              close char-str)))
    (unwind-protect
        (save-excursion
          (goto-char (overlay-start overlay))
          (insert open)
          (goto-char (overlay-end overlay))
          (insert close))
      (delete-overlay overlay))))

(defun embrace--delete (char &optional change-p)
  (let* ((struct (assoc-default char embrace--pairs-list))
         (open (embrace-pair-struct-left-regexp struct))
         (close (embrace-pair-struct-right-regexp struct))
         (overlay (embrace--get-region-overlay open close)))
    (unless overlay
      (error "No such a pair found"))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char (overlay-start overlay))
            (when (looking-at open)
              (delete-char (string-width (match-string 0))))
            (goto-char (overlay-end overlay))
            (when (looking-back close)
              (backward-delete-char (string-width (match-string 0)))))
          (when change-p overlay))
      (unless change-p (delete-overlay overlay)))))

(defun embrace--internal (change-p)
  (let* ((char (read-char "Delete pair: "))
         (overlay (embrace--delete char change-p)))
    (and change-p
         (overlayp overlay)
         (embrace--insert (read-char "Insert pair: ") overlay))))

;;;###autoload
(defun embrace-delete ()
  (interactive)
  (embrace--internal nil))

;;;###autoload
(defun embrace-change ()
  (interactive)
  (embrace--internal t))

;;;###autoload
(defun embrace-add ()
  (interactive)
  (let ((mark-func (assoc-default (read-char "Semantic unit: ")
                                  embrace-semantic-units-alist))
        overlay)
    (unless (fboundp mark-func)
      (error "No such a semantic unit"))
    (save-excursion
      (funcall mark-func)
      (setq overlay (make-overlay (region-beginning) (region-end) nil nil t))
      (embrace--insert (read-char "Add pair: ") overlay)
      (delete-overlay overlay))))

;;;###autoload
(defun embrace-commander ()
  (interactive)
  (let ((char (read-char "Command [acd]: ")))
    (cond
     ((eq char ?a)
      (call-interactively 'embrace-add))
     ((eq char ?c)
      (call-interactively 'embrace-change))
     ((eq char ?d)
      (call-interactively 'embrace-delete))
     (t
      (error "Unknow key")))))

;;; `evil-surround' integration
(defun embrace-evil-surround-delete (char &optional outer inner)
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    (if (member char embrace-evil-surround-key)
        (let* ((outer (evil-surround-outer-overlay char))
               (inner (evil-surround-inner-overlay char)))
          (unwind-protect
              (when (and outer inner)
                (evil-surround-delete char outer inner))
            (when outer (delete-overlay outer))
            (when inner (delete-overlay inner))))
      (embrace--delete char)))))

(defun embrace-evil-surround-change (char &optional outer inner)
  (interactive "c")
  (let (overlay)
    (cond
     ((and outer inner)
      (evil-surround-delete char outer inner)
      (let ((key (read-char)))
        (if (member key embrace-evil-surround-key)
            (evil-surround-region (overlay-start outer)
                                  (overlay-end outer)
                                  nil (if (evil-surround-valid-char-p key) key char))
          (embrace--insert key (copy-overlay outer)))))
     (t
      (if (member char embrace-evil-surround-key)
          (let* ((outer (evil-surround-outer-overlay char))
                 (inner (evil-surround-inner-overlay char)))
            (unwind-protect
                (when (and outer inner)
                  (evil-surround-change char outer inner))
              (when outer (delete-overlay outer))
              (when inner (delete-overlay inner))))
        (setq overlay (embrace--delete char t))
        (let ((key (read-char)))
          (if (member key embrace-evil-surround-key)
              (evil-surround-region (overlay-start overlay)
                                    (overlay-end overlay)
                                    nil (if (evil-surround-valid-char-p key) key char))
            (embrace--insert key overlay))
          (when overlay (delete-overlay overlay))))))))

;;;###autoload
(defun embrace-enable-evil-surround-integration ()
  (interactive)
  (when (require 'evil-surround nil t)
    (advice-add 'evil-surround-change :override 'embrace-evil-surround-change)
    (advice-add 'evil-surround-delete :override 'embrace-evil-surround-delete)))

;;;###autoload
(defun embrace-disable-evil-surround-integration ()
  (interactive)
  (when (require 'evil-surround nil t)
    (advice-remove 'evil-surround-change 'embrace-evil-surround-change)
    (advice-remove 'evil-surround-delete 'embrace-evil-surround-delete)))

(provide 'embrace)
;;; embrace.el ends here
