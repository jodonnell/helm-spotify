;;; helm-spotify.el --- Control Spotify with Helm.
;; Copyright 2013 Kris Jenkins
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: helm spotify
;; URL: https://github.com/krisajenkins/helm-spotify
;; Created: 14th October 2013
;; Version: 0.1.1
;; Package-Requires: ((helm "0.0.0") (multi "2.0.0"))

;;; Commentary:
;;
;; A search & play interface for Spotify.
;;
;; Currently supports OSX, Linux & Windows.
;;
;; (Want support for another platform? There's a guide in the github README.)

;;; Code:

;;; API Reference: https://developer.spotify.com/technologies/web-api/
(require 'url)
(require 'json)
(require 'helm)
(require 'multi)

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defmulti spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  system-type)

(defmulti-method spotify-play-href 'darwin
  (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defmulti-method spotify-play-href 'gnu/linux
  (href)
  (shell-command (format "dbus-send --session --type=method_call --dest=com.spotify.qt / org.freedesktop.MediaPlayer2.OpenUri \"string:%s\""
			 href)))

(defmulti-method spotify-play-href 'windows-nt
  (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback spotify-play-href
  (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (alist-get '(uri) track)))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (spotify-play-href (alist-get '(album uri) track)))

(defun spotify-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "https://api.spotify.com/v1/search?type=track&q=%s" search-term)))
    (let ((inhibit-message t))
      (with-current-buffer
        (url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read)))))

(defun spotify-track-name (track)
  "Get the name of the TRACK."
  (alist-get '(name) track))

(defun spotify-track-duration (track)
  "Get the duration of the TRACK."
  (alist-get '(duration_ms) track))

(defun spotify-track-album-name (track)
  "Get the album name of the TRACK."
  (alist-get '(album name) track))

(defun spotify-track-artist-name (track)
  "Get the artist names of the TRACK."
  (mapcar (lambda (artist)
            (alist-get '(name) artist))
          (alist-get '(artists) track)))

(defun spotify-format-artist-name (names)
  "Format the artist NAMES."
  (mapconcat 'identity names "/"))

(defun spotify-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name      (spotify-track-name track))
        (track-length-ms (spotify-track-duration track))
        (album-name      (spotify-track-album-name track))
        (artist-names    (spotify-track-artist-name track)))

    (let ((track-length (/ track-length-ms 1000)))
      (format "%s (%dm%0.2ds) %s - %s"
              track-name
              (/ track-length 60) (mod track-length 60)
              (spotify-format-artist-name artist-names)
              album-name))))


(defun spotify-format-album (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((album-name      (spotify-track-album-name track))
        (artist-names    (spotify-track-artist-name track)))
    (format "%s - %s"
            (spotify-format-artist-name artist-names)
            album-name)))

(defun spotify-search-formatted (search-term format-function)
  "Do the search on SEARCH-TERM to spotify and return a correctly formatted list based on FORMAT-FUNCTION."
  (spotify-unique-list
   (mapcar (lambda (track)
             (cons (funcall format-function track) track))
           (alist-get '(items) (alist-get '(tracks) (spotify-search search-term))))))

(defun spotify-unique-list (tracks)
  "Given a list of TRACKS, unique them."
  (if (not tracks)
      '()
    (let ((track (car tracks)))
      (let ((format-string (car track)))
        (cons track (spotify-unique-list
                     (-remove (lambda (other-track)
                                (string= (car other-track) format-string))
                                (cdr tracks))))))))

(defun helm-spotify-pattern ()
  "Adds a wildcard to the end of the pattern"
  (concat helm-pattern "*"))


(defun helm-spotify-search-track ()
  "Run the search by track."
  (spotify-search-formatted (helm-spotify-pattern) 'spotify-format-track))

(defun helm-spotify-search-album ()
  "Run the search by album."
  (spotify-search-formatted (helm-spotify-pattern) 'spotify-format-album))

(defun helm-spotify-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track))       . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

(defun helm-spotify-actions-for-album (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
      ("Show Track Metadata" . pp)))

;;;###autoload
(defvar helm-source-spotify-track-search
  '((name . "Spotify Track")
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (candidates-process . helm-spotify-search-track)
    (action-transformer . helm-spotify-actions-for-track)))

(defvar helm-source-spotify-album-search
  '((name . "Spotify Artist")
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (candidates-process . helm-spotify-search-album)
    (action-transformer . helm-spotify-actions-for-album)))

;;;###autoload
(defun helm-spotify-track ()
  "Bring up a Spotify track search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spotify-track-search)
	:buffer "*helm-spotify-track*"))

(defun helm-spotify-artist ()
  "Bring up a Spotify artist search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spotify-album-search)
	:buffer "*helm-spotify-album*"))

(provide 'helm-spotify)
;;; helm-spotify.el ends here
