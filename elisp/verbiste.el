;;; verbiste.el --- word conjugation

;;; Commentary:

;; Ben Voui wrote the first verbiste.el. This one is a completely
;; different. This mode displays a list of conjugation of a french
;; verb in a nice table. To use install it in your load-path. You then
;; put the following in your .emacs:
;;
;; (require 'verbiste)
;;
;; Put your cursor on a verb and M-x verbiste will display a table of
;; conjugation.

;; Ben Voui a écrit le premier verbiste.el. Cette version est
;; complètement différente. Ce mode affiche une liste de conjugaison
;; dans un joli tableau. Pour utiliser ce mode installer ce fichier
;; dans un chemin de load-path. Ensuite mettez la ligne suivante dans
;; votre .emacs :
;;
;; (require 'verbiste)
;;
;; Mettez votre curseur sur un verbe puis M-x verbiste vous afficher
;; un tableau de conjugaison.

;;; THANKS:

;; Pierre Sarrazin for the excellent verbiste program

;;; BUGS:

;;; Code:

(defvar verbiste-buffer "*verbiste*"
  "Name of the buffer where conjugated verbs will be displayed")

;;;###autoload
(defun verbiste ()
  "Display conjugation of verb at point"
  (interactive)
  (let* ((word (car (last (split-string (current-word) "'"))))
         (verb (read-string "Conjugaison du verbe : "
                            word nil word))
         (split (verbiste-split verb))
         string)
    (if split
        (verbiste-display split)
        (setq string (shell-command-to-string
                      (concat "french-deconjugator " verb)))
        (if (string= string "\n")
            (message "Pas de conjugaison pour %s." verb)
            (setq split (verbiste-split (car (split-string string ","))))
            (verbiste-display split)))))

(defun verbiste-split (word)
  "Return a list of conjugations for WORD"
  (let (string ret line)
    (with-temp-buffer
        (call-process "french-conjugator" nil t nil word)
      (goto-char (point-min))
      (while (not (eobp))
             (setq line (buffer-substring
                         (line-beginning-position) (line-end-position)))
             (if (not (or (string= line "")
                          (string= (substring line 0 1) "-")))
                 (setq string (append string (list line)))
                 (when (not (bobp))
                   (setq ret (append ret (list string)))
                   (setq string nil)))
             (forward-line 1)))
    ret))

(defun verbiste-wide (split)
  "Return t if one of the conjugate is larger than 11 characters."
  (let ((list (apply 'append split))
        (wide nil))
    (while list
           (when (> (length (car list)) 11)
             (setq wide t))
           (setq list (cdr list)))
    wide))

(defun verbiste-display (split)
  "Display SPLIT into a buffer"
  (let ((buf (get-buffer-create verbiste-buffer))
        (wide (verbiste-wide split))
        verb)
    (with-help-window
        buf
      (with-current-buffer buf
        (kill-region (point-min) (point-max))
        (if wide
            (progn
              (verbiste-first-wide-banner)
              (verbiste-list-tense 0 4 split 20)
              (verbiste-second-wide-banner)
              (verbiste-list-tense 4 8 split 20)
              (verbiste-third-wide-banner)
              (verbiste-list-tense 8 11 split 20))
            (verbiste-first-banner)
            (verbiste-list-tense 0 6 split 12)
            (verbiste-second-banner)
            (verbiste-list-tense 6 11 split 12)
            (display-buffer buf t))))))

(defun verbiste-list-tense (vls-start vls-end split vbl-len)
  "List all tenses from VLS-START to VLS-END."
  (let ((tense 0)
        (declination 0)
        (space-pad (make-string vbl-len ?\s))
        verb)
    (while (< declination 6)
           (setq tense vls-start)
           (while (< tense vls-end)
                  (setq verb (nth declination (nth tense split)))
                  (if verb
                      (insert (verbiste-pad-string verb vbl-len))
                      (insert space-pad))
                  (setq tense (1+ tense)))
           (insert "\n")
           (setq declination (1+ declination)))))

(defun verbiste-first-banner ()
  "Display the first banner."
  (verbiste-banner
   (concat
    "infinitif   present     imparfait   futur       passé       conditionnel\n"
    "                                                            présent\n")))

(defun verbiste-second-banner ()
  "Display the second banner"
  (verbiste-banner
   (concat
    "\n"
    "subjonctif  subjonctif  imperatif   participe   participe\n"
    "présent     imparfait   présent     présent     passé\n")))

(defun verbiste-first-wide-banner ()
  "Display the first wide banner."
  (verbiste-banner
   (concat
    "infinitif           present             imparfait           futur\n")))

(defun verbiste-second-wide-banner ()
  "Display the second wide banner."
  (verbiste-banner
   (concat
    "\n"
    "passé               conditionnel        subjonctif          subjonctif\n"
    "                    présent             présent             imparfait\n")))

(defun verbiste-third-wide-banner ()
  "Display the third wide banner."
  (verbiste-banner
   (concat
    "\n"
    "imperatif présent   participe présent   participe passé\n")))

(defun verbiste-banner (string)
  "Common function for displaying banner, set face to bold"
  (insert (propertize string 'face 'bold)))

(defun verbiste-pad-string (string padding)
  "Pad a string for nice column display."
  (let ((len (length string)))
    (if (< len padding)
        (concat string (make-string (- padding len) ?\s))
        (substring string (- len padding)))))

(provide 'verbiste)

;; Local Variables:
;; compile-command: "make"
;; End:

;; Copyright (C) 2010 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
