;;; smart-tabs-mode.el --- Intelligently indent with tabs, align with spaces!

;; Copyright © 2011 John Croisant <jacius@gmail.com>
;; Copyright © 2011 Joel C. Salomon <joelcsalomon@gmail.com>
;; Copyright © 2012 Alan Pearce <alan@alanpearce.co.uk>

;; Author: John Croisant <jacius@gmail.com>
;;         Joel C. Salomon <joelcsalomon@gmail.com>
;;         Alan Pearce <alan@alanpearce.co.uk>
;; URL: http://www.emacswiki.org/emacs/SmartTabs
;; Created: 19 Sep 2011
;; Version: 0.1
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provide a semantic way of using tab characters in
;; source code: tabs for indentation, spaces for alignment.

;;; Installation:

;; To use, save smart-tabs-mode.el to a a directory on your load-path
;; (e.g., ~/.emacs.d/elisp), then add the following to your .emacs file:
;;
;;  (autoload 'smart-tabs-mode "smart-tabs-mode"
;;    "Intelligently indent with tabs, align with spaces!")
;;  (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;  (autoload 'smart-tabs-advice "smart-tabs-mode")
;;
;; Then, for each language you want to use smart tabs, set up a hook
;; and advice like so:
;;
;;  (add-hook 'MODE-HOOK 'smart-tabs-mode-enable)
;;  (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)
;;
;; Note that it might be preferable to delay calling smart-tabs-advice
;; until after the major mode is loaded and evaluated:
;;
;; (eval-after-load 'MODE-FEATURE
;;   '(smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR))
;;
;; Or:
;;
;;  (add-hook 'MODE-HOOK (lambda ()
;;                         (smart-tabs-mode-enable)
;;                         (smart-tabs-advice INDENT-FUNC TAB-WIDTH-VAR)))
;;
;; Here are some specific examples for a few popular languages:
;;
;;  ;; C/C++
;;  (add-hook 'c-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice c-indent-line c-basic-offset)
;;  (smart-tabs-advice c-indent-region c-basic-offset)
;;
;;  ;; JavaScript
;;  (add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice js2-indent-line js2-basic-offset)
;;
;;  ;; Perl (cperl-mode)
;;  (add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice cperl-indent-line cperl-indent-level)
;;
;;  ;; Python
;;  (add-hook 'python-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice python-indent-line-1 python-indent)
;;
;;  ;; Ruby
;;  (add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
;;  (smart-tabs-advice ruby-indent-line ruby-indent-level)
;;
;; This package is derived from <http://www.emacswiki.org/emacs/SmartTabs>
;; as modified by John Croisant (jacius), along with Julien Fontanet and
;; Tomita Hiroshi (tomykaira).
;;
;; Modification history is at <https://github.com/jcsalomon/smarttabs>.

;;; Code:

(require 'advice)

(defmacro smart-tabs-mode/no-tabs-mode-advice (function)
  `(unless (ad-find-advice ',function 'around 'smart-tabs)
     (defadvice ,function (around smart-tabs activate)
       (if smart-tabs-mode
           (let ((indent-tabs-mode nil)) ad-do-it)
         ad-do-it))))

;;;###autoload
(define-minor-mode smart-tabs-mode
  "Intelligently indent with tabs, align with spaces!"

  (progn
    (smart-tabs-mode/no-tabs-mode-advice align)
    (smart-tabs-mode/no-tabs-mode-advice align-regexp)
    (smart-tabs-mode/no-tabs-mode-advice indent-relative)

    (unless
        (ad-find-advice 'indent-according-to-mode 'around 'smart-tabs)
      (defadvice indent-according-to-mode (around smart-tabs activate)
        (if smart-tabs-mode
            (let ((indent-tabs-mode indent-tabs-mode))
              (if (memq indent-line-function
                        '(indent-relative
                          indent-relative-maybe))
                  (setq indent-tabs-mode nil))
              ad-do-it)
          ad-do-it)))
    ))

;;;###autoload
(defun smart-tabs-mode-enable ()
  "Enable smart-tabs-mode."
  (smart-tabs-mode t))

;;;###autoload
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (smart-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((indent-tabs-mode t)
               (tab-width fill-column)
               (,offset fill-column))
           (unwind-protect
               (progn ad-do-it))))
        (t
         ad-do-it)))))


(provide 'smart-tabs-mode)

;;; smart-tabs-mode.el ends here
