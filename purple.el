;;; purple.el --- Purple for Emacs

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords:

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

(require 'dbus)

(require 'purple-buddy)
(require 'purple-chat)
(require 'purple-mail)

(defgroup purple nil
  "Activity management group"
  :group 'applications)

(defcustom purple-dbus-service "im.pidgin.purple.PurpleService"
  "Purple dbus service name. Default value is for pidgin."
  :group 'purple)

(defcustom purple-object "/im/pidgin/purple/PurpleObject"
  "Purple object name. Default value is for pidgin."
  :group 'purple)

(defcustom purple-interface "im.pidgin.purple.PurpleInterface"
  "Purple interface name. Default value is for pidgin."
  :group 'purple)

(defvar purple-accounts '())

;; Shared
(defmacro purple-call-method (method &rest args)
  `(dbus-call-method :session purple-dbus-service
		     purple-object purple-interface ,method ,@args))

(defun purple-call-method-async (method handler &rest args)
  (apply 'dbus-call-method-asynchronously :session purple-dbus-service
	 purple-object purple-interface
	 method handler args))

(defsubst purple-register-signal (sig fun)
  (dbus-register-signal :session purple-dbus-service
			purple-object purple-interface sig fun))

(defun purple-register-signals (signals)
  (dolist (sig signals)
    (purple-register-signal (car sig) (cdr sig))))

;; Init
(defun purple-init ()
  "Initialize purple for Emacs."
  (interactive)
  (setq purple-accounts (purple-account-list))
  (unless purple-accounts
    (error "Purple seems turned off."))
  (mapc 'purple-buddy-init-for-account purple-accounts))

(defun purple-account-list ()
  (purple-call-method "PurpleAccountsGetAllActive"))

;; Tools
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(provide 'purple)
