;;; A Emacs plugin for using Youdao Translate API
;; youdao-translate.el ---

;; Copyright (C) 2014  lu4nx

;; Author: lu4nx <lx@shellcodes.org>
;; URL: https://github.com/1u4nx/youdao-translate.el
;; Created: 24th October 2014
;; Version: v0.1
;; License: GPLv3
;; Keywords: elisp, youdao, translate


;;; Code:

(require 'cl)
(require 'json)

(defun youdao-translate-word ()
  "查询被mark的单词"
  (interactive)
  (let* ((mark-pos (mark))
         (point-pos (point))
         (word (buffer-substring-no-properties mark-pos point-pos)))
    (youdao-online-translate word)))

(defun youdao-input->translate (word)
  "查询用户输入的单词"
  (interactive "sInput a word: ")
  (youdao-online-translate word))

;; TODO: 对于长句子, 格式化的方法需要调整
(defun show-translate-result (basic-data)
  (when (not basic-data)
      (error "Not found"))
  (message
   (with-output-to-string
     (princ (format "英式发音：%s\n美式发音：%s\n"
                    (cdr (assoc 'uk-phonetic basic-data))
                    (cdr (assoc 'us-phonetic basic-data))))
     (princ "基本释义：\n")
     (loop for explain across (cdr (assoc 'explains basic-data))
           do
           (princ (format "%s\n" explain))))))

(defun url->content (url)
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (kill-line)
    (buffer-string)))

(defun youdao-online-translate (word)
  (let* ((api-url "http://openapi.youdao.com/api")
	 (salt (random t))
	 (from "auto")
	 (to "auto")
	 (curtime (time-convert (current-time) 'integer))
	 (inputStr (if (> (string-width word) 20)
		       (format "%s%d%s" (substring word 0 10) (string-width word) (substring word -10))
		     word))
	 (signStr (format "%s%s%s%s%s" appid inputStr salt curtime appsecret))
	 (sign (secure-hash 'sha256 signStr))
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type: application/x-www-form-urlencoded; charset=utf-8")))
	 (url-request-data
	  (url-encode-url (format "q=%s&from=%s&to=%s&appKey=%s&salt=%s&sign=%s&signType=v3&curtime=%s"
		  word from to appid salt sign curtime)))
         (url-data (decode-coding-string (url->content api-url) 'utf-8))
         (json-data (json-read-from-string url-data)))
    (show-translate-result (cdr (assoc 'basic json-data)))))

(provide 'youdao-translate)

;;; youdao-translate.el ends here
