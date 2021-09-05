;; -*- lexical-binding: t -*-

;; Copyright (C) 2021 Lazju

;; Author: Lazju
;; Keywords: tools
;; URL:
;; Version: 0.0.1

;; Usage:
;; Open your chrome with remote debugging enabled:
;;     open -a "Google Chrome" --args "--remote-debugging-port=9222"
;; Require you to use Org-roam to take notes, the url should be stored in the "ROAM-REFS" property,
;; call "web-noter-insert-heading" to insert a heading, "web-noter-sync-from-heading" to make a video jump to corresponding position.

(require 'websocket)
(require 'json)
(require 'url)
(require 's)
(require 'dash)
(require 'cl-generic)
(require 'org)

;; Variables

(defvar-local web-noter--browser-handler nil)
(defvar-local web-noter--website-handler nil)
(defvar web-noter--msg-id 0)

(defcustom web-noter-browser 'google-chrome
  "backend"
  :group 'web-noter)

;; Utility functions

(defun web-noter--json-encode (p)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode p)))

(defun web-noter--json-decode (p)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string p)))

(defun web-noter--get-ref ()
  (org-entry-get (point-min) "ROAM_REFS"))

(defun web-noter--get-msg-id ()
  (setq web-noter--msg-id (+ 1 web-noter--msg-id)))

(defun web-noter--determine-website (url)
  (cond
   ((s-starts-with? "https://www.youtube.com" url) (make-web-noter--youtube))))

(defun web-noter--second-to-hms (sec)
  "Convert seconds to H:m:s format"
  (when (not (numberp sec))
    (setq sec (string-to-number sec)))
  (let* ((hour (/ sec 3600))
           (minute (/ (mod sec 3600) 60))
           (second (- sec (* hour 3600) (* minute 60))))
      (format "%02d:%02d:%02d" hour minute second)))

(defun web-noter--hms-to-second (hms)
  (let ((splited (-map 'string-to-number (s-split ":" hms))))
      (+ (* (first splited) 3600)
         (* (second splited) 60)
         (third splited))))

;; Hooks

(let ((obarray '()))
  (defun web-noter--add-hook (id func)
    (let ((hook (intern (number-to-string id) obarray)))
      (add-hook hook func t)))

  (defun web-noter--run-hook (id data)
    (let ((hook (intern-soft (number-to-string id) obarray)))
      (when hook
        (run-hook-with-args hook data)
        (unintern hook obarray)))))

;; Browsers

(cl-defstruct web-noter--google-chrome
  (host "127.0.0.1")
  (port 9222)
  (socket nil))

(cl-defgeneric web-noter--open (browser url))

(cl-defmethod web-noter--open ((browser web-noter--google-chrome) url)
  (cond ((eq system-type 'darwin)
         (shell-command (concat "open -a \"/Applications/Google Chrome.app\" \""
                                url
                                "\""))))
  (if-let* ((host (web-noter--google-chrome-host browser))
            (port (web-noter--google-chrome-port browser))
            (tabs (let* ((url-request-method "GET")
                         (url-http-attempt-keepalives nil)
                         (url (url-parse-make-urlobj
                               "http" nil nil host port "/json")))
                    (with-current-buffer (url-retrieve-synchronously url)
                      (if (eq 200 (url-http-parse-response))
                          (progn
                            (goto-char (+ 1 url-http-end-of-headers))
                            (let ((json-array-type 'list)
                                  (json-object-type 'plist))
                              (json-read)))
                        (message "Cannot connect to %s:%d" host port)
                        nil))))
            (ws-url (first
                     (-map
                      (lambda (tab)
                        (plist-get tab :webSocketDebuggerUrl))
                      (-filter (lambda (tab)
                                 (string-equal url (plist-get tab :url)))
                               tabs))))
            (buf (current-buffer)))
      (websocket-open ws-url
                      :on-open
                      (lambda (ws)
                        (with-current-buffer buf
                          (message "Socket opened")
                          (setf (web-noter--google-chrome-socket browser) ws)
                          (setq web-noter--browser-handler browser)
                          (web-noter--send browser "Debugger.enable")))
                      :on-message
                      (lambda (ws frame)
                        (with-current-buffer buf
                        (let* ((data (web-noter--json-decode
                                      (websocket-frame-payload frame)))
                               (id (plist-get data :id)))
                          (when (numberp id)
                              (web-noter--run-hook id data)))))
                      :on-close
                      (lambda (ws)
                        (with-current-buffer buf
                        (message "Socket closed")
                        (setf (web-noter--google-chrome-socket browser) nil))
                        (setq web-noter--browser-handler nil)))
    (message "Open socket failed")))


(cl-defgeneric web-noter--close (browser))

(cl-defmethod web-noter--close ((browser web-noter--google-chrome))
  (websocket-close (web-noter--google-chrome-socket browser))
  (setf (web-noter--google-chrome-socket browser) nil)
  (setq web-noter--browser-handler nil))


(cl-defgeneric web-noter--send
    (browser method &optional params callback)
  "Send message to BROWSER")

(cl-defmethod web-noter--send
  ((browser web-noter--google-chrome)
   method &optional params callback)
  (let ((id (web-noter--get-msg-id)))
    (when callback
      (web-noter--add-hook id callback))
    (websocket-send-text (web-noter--google-chrome-socket browser)
                         (web-noter--json-encode (list :id id
                                                  :method method
                                                  :params params)))))

(defun web-noter--google-chrome-rpc-decode (data)
  (let* ((res (plist-get data :result))
         (was-thrown (plist-get res :wasThrown))
         (result (plist-get res :result))
         (type (plist-get result :type))
         (value (plist-get result :value))
         (descr (plist-get result :description)))
    (let ((msg (if (eq json-false was-thrown)
                   (if (string-equal "object" type)
                       (wooky--encode value) (or value descr type))
                 descr)))
      (when msg (message "%s" msg)))))

(cl-defgeneric web-noter--inject (browser code &optional callback)
  "Inject JS code into BROWSER with callback CALLBACK to be executed later")

(cl-defmethod web-noter--inject ((browser web-noter--google-chrome)
                                 code &optional callback)
  (web-noter--send browser "Runtime.evaluate"
                   (list :expression code
                         :returnByValue t)
                   (lambda (data)
                     (let ((return-value (web-noter--google-chrome-rpc-decode data)))
                       (if callback
                           (funcall callback return-value)
                         return-value)))))

;; Websites

(cl-defstruct web-noter--youtube
  "Timestamp format: 00:00:00"
  (get-current-time ))


(cl-defgeneric web-noter--get-current-pos (website callback))

(cl-defmethod web-noter--get-current-pos
  ((website web-noter--youtube) callback)
  (web-noter--inject web-noter--browser-handler
                     "Math.floor(document.getElementById('movie_player').getCurrentTime())"
                     (lambda (sec)
                       (funcall callback (web-noter--second-to-hms sec)))))


(cl-defgeneric web-noter--seek-to (website pos))

(cl-defmethod web-noter--seek-to
  ((website web-noter--youtube) pos)
  (if (string-match "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}$" pos)
      (progn
       (let ((sec (web-noter--hms-to-second pos)))
         (web-noter--inject web-noter--browser-handler
                     (format "document.getElementById('movie_player').seekTo(%d)"
                             sec))))
    (message "No valid pos in current line")))

;; Browser & Website

(defun web-noter-do-with-current-pos (do)
  (web-noter--get-current-pos web-noter--website-handler do))

(defun web-noter-seek-to (pos)
  (web-noter--seek-to web-noter--website-handler pos))

;; Org

(defun web-noter-insert-heading ()
  (interactive)
  (web-noter-do-with-current-pos
   (lambda (pos)
     (org-insert-heading)
     (insert pos))))

(defun web-noter-sync-from-heading ()
  (interactive)
  (web-noter-seek-to (first (s-split " " (org-entry-get nil "ITEM")))))

;; Mode

(defun web-noter-mode-enter ()
  (if-let* ((browser (cond
                      ((eq web-noter-browser 'google-chrome) (make-web-noter--google-chrome))))
            (url (web-noter--get-ref))
            (website (web-noter--determine-website url)))
      (progn
        (add-hook 'kill-buffer-hook 'web-noter-mode-exit nil t)
        (setq web-noter--website-handler website)
        (web-noter--open browser url)
        (message "Web noter mode entered."))
    (message "Enter web noter mode failed.")))

(defun web-noter-mode-exit ()
  (remove-hook 'kill-buffer-hook 'web-noter-mode-exit t)
  (when web-noter--browser-handler
    (web-noter--close web-noter--browser-handler))
  (when web-noter--website-handler
    (setq web-noter--website-handler nil))
    (message "Web noter mode exited."))

;;;###autoload
(define-minor-mode web-noter-mode
  "Minor mode for taking notes of references from browsers."
  :global nil
  :group 'web-noter
  :init-value nil
  :lighter " WN"
  (if web-noter-mode
      (web-noter-mode-enter)
    (web-noter-mode-exit)))

(provide 'web-noter)
