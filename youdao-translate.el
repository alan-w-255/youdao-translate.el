;;; youdao-translate.el ---

;; Copyright (C) 2014  lu4nx

;; Author: lu4nx <lx@shellcodes.org>
;; Date: 2014-10-24

(require 'cl)
(require 'json)

(defun youdao-translate-word ()
  "查询被mark的单词"
  (interactive)
  (youdao-online-translate (buffer-substring-no-properties (mark) (point))))

(defun youdao-input->translate (word)
  "查询用户输入的单词"
  (interactive "sInput a word: ")
  (youdao-online-translate word))

(defun show-basic-result (basic-data)
  (message
   (if basic-data
       (with-output-to-string
         (princ (format "英式发音：%s\n美式发音：%s\n"
                        (cdr (assoc 'uk-phonetic basic-data))
                        (cdr (assoc 'us-phonetic basic-data))))
         (princ "基本释义：\n")
         (loop for explain across (cdr (assoc 'explains basic-data))
               do
               (princ (format "%s\n" explain))))
     (princ "Not found"))))

(defun url->content (url)
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (kill-line)
    (buffer-string)))

(defun youdao-online-translate (word)
  (let* ((api-url (format "http://fanyi.youdao.com/openapi.do?keyfrom=%s&key=%s&version=1.1&doctype=json&type=data&q=%s"
                          youdao-api-keyfrom
                          youdao-api-key
                          word))
         (url-data (decode-coding-string (url->content api-url) 'utf-8))
         (json-data (json-read-from-string url-data)))
    (show-basic-result (cdr (assoc 'basic json-data)))))

(provide 'youdao-translate)
