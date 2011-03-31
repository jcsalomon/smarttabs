;; Emacs smart tabs functionality
;;   Intelligently indent with tabs, align with spaces!
;;
;; Note: Indenting only uses tabs when indent-tabs-mode is non-nil,
;; otherwise it uses spaces as usual.
;;
;; To use: save as smarttabs.el in your .emacs.d directory, and add
;; "(require 'smarttabs)" to your .emacs file.
;;
;; Code is GPLv2, derived from http://www.emacswiki.org/emacs/SmartTabs
;;
;; Modifications by John Croisant:
;;  * Remembers and re-applies window start, so that indenting doesn't
;;    cause the window to visibly scroll. (2009-09-18)


(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
         (beginning-of-line)
         (while (looking-at "\t*\\( +\\)\t+")
           (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
             (,offset fill-column)
             (wstart (window-start)))
         ad-do-it
         (set-window-start (selected-window) wstart)))
      (t
       ad-do-it))))

;; C/C++
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

;; JavaScript
(smart-tabs-advice js2-indent-line js2-basic-offset)

;; Perl
(smart-tabs-advice cperl-indent-line cperl-indent-level)

;; Python
(smart-tabs-advice python-indent-line-1 python-indent)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
			(setq tab-width (default-value 'tab-width))))

;; Ruby
(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)

;; VHDL
(smart-tabs-advice vhdl-indent-line vhdl-basic-offset)
(setq vhdl-indent-tabs-mode t)


(provide 'smarttabs)
