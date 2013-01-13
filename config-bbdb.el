;; config-bbdb.el
;; Copyright (C) 2011, 2012 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar bbdb-dir (concat src-dir "bbdb/"))
(fni/add-to-load-path bbdb-dir t t)
(setq bbdb-file (concat dotfiles-dir "bbdb")) ;; keep ~/ clean; set before loading
(add-to-list 'Info-default-directory-list
             (expand-file-name (concat bbdb-dir
                                       "texinfo/")))

(require 'bbdb)
(bbdb-initialize 'gnus)

(bbdb-insinuate-w3)

(setq
 bbdb-offer-save 1                        ;; 1 means save-without-asking

 gnus-score-find-score-files-function
 '(gnus-score-find-bnews bbdb/gnus-score)

 bbdb-use-pop-up t                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small

 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

 bbdb-always-add-address t                ;; add new addresses to existing...
 ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

 bbdb-completion-type nil                 ;; complete on anything

 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 ;; this only works partially

 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA

 bbdb-elided-display t                    ;; single-line addresses

 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

(provide 'config-bbdb)

