;; youdao-translate.el ---  A Emacs plugin for using Youdao Translate API -*- lexical-binding: t; -*-

;; Copyright (C) 2014  lu4nx

;; Author: lu4nx <lx@shellcodes.org>
;; URL: https://github.com/1u4nx/youdao-translate.el
;; Created: 24th October 2014
;; Version: v0.1
;; License: GPLv3
;; Keywords: elisp, youdao, translate


;;; Commentary:

;;; Code:

(require 'cl)
(require 'json)
(require 'url)

(defcustom youdao-translate-show-phonetic t
  "Show phonetic symbol or not."
  :type 'boolean)

(defun youdao-translate-region ()
  "查询被选中的单词"
  (interactive)
  (when (use-region-p)
    (let ((word (buffer-substring-no-properties (use-region-beginning) (use-region-end))))
      (youdao-online-translate word))))

(defun youdao-input->translate (word)
  "查询用户输入的单词."
  (interactive "sInput a word: ")
  (youdao-online-translate word))

(defun show-translate-result (json-data)
  "显示翻译结果."
  (if (equal (cdr (assoc 'isWord json-data)) :json-false)
      (with-output-to-string
	(princ "翻译:")
	(cl-loop for translation across (cdr (assoc 'translation json-data))
		 do
		 (princ (format "\n%s" translation))))
    (when-let ((basic-data (cdr (assoc 'basic json-data))))
      (with-output-to-string
	(when youdao-translate-show-phonetic
	  (princ (format "英式发音：%s\n美式发音：%s\n"
			 (cdr (assoc 'uk-phonetic basic-data))
			 (cdr (assoc 'us-phonetic basic-data)))))
	(princ "基本释义:")
	(cl-loop for explain across (cdr (assoc 'explains basic-data))
		 do
		 (princ (format "\n%s" explain)))))))

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
    (show-translate-result json-data)))

(defun youdao-translate-at-point ()
  "查询光标下的单词."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (message (youdao-online-translate word))))

(defun youdao-translate ()
  "查询光标下的单词或者被选中的句子."
  (interactive)
  (if (use-region-p)
      (youdao-translate-region)
    (message (youdao-translate-at-point))))

(provide 'youdao-translate)

;;; youdao-translate.el ends here
