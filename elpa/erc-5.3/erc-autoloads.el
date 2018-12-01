;;; erc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "erc" "erc.el" (23323 26118 124546 348000))
;;; Generated autoloads from erc.el

(autoload 'erc-select-read-args "erc" "\
Prompt the user for values of nick, server, port, and password.

\(fn)" nil nil)

(autoload 'erc "erc" "\
ERC is a powerful, modular, and extensible IRC client.
This function is the main entry point for ERC.

It permits you to select connection parameters, and then starts ERC.

Non-interactively, it takes the keyword arguments
   (server (erc-compute-server))
   (port   (erc-compute-port))
   (nick   (erc-compute-nick))
   password
   (full-name (erc-compute-full-name)))

That is, if called with

   (erc :server \"irc.freenode.net\" :full-name \"Harry S Truman\")

then the server and full-name will be set to those values, whereas
`erc-compute-port', `erc-compute-nick' and `erc-compute-full-name' will
be invoked for the values of the other parameters.

\(fn &key (server (erc-compute-server)) (port (erc-compute-port)) (nick (erc-compute-nick)) PASSWORD (full-name (erc-compute-full-name)))" t nil)

(defalias 'erc-select 'erc)

(autoload 'erc-handle-irc-url "erc" "\
Use ERC to IRC on HOST:PORT in CHANNEL as USER with PASSWORD.
If ERC is already connected to HOST:PORT, simply /join CHANNEL.
Otherwise, connect to HOST:PORT as USER and /join CHANNEL.

\(fn HOST PORT CHANNEL USER PASSWORD)" nil nil)

;;;***

;;;### (autoloads nil "erc-autoaway" "erc-autoaway.el" (23323 26117
;;;;;;  967880 137000))
;;; Generated autoloads from erc-autoaway.el
 (autoload 'erc-autoaway-mode "erc-autoaway")

;;;***

;;;### (autoloads nil "erc-bbdb" "erc-bbdb.el" (23323 26117 954546
;;;;;;  842000))
;;; Generated autoloads from erc-bbdb.el
 (autoload 'erc-bbdb-mode "erc-bbdb")

;;;***

;;;### (autoloads nil "erc-button" "erc-button.el" (23323 26117 861213
;;;;;;  780000))
;;; Generated autoloads from erc-button.el
 (autoload 'erc-button-mode "erc-button" nil t)

;;;***

;;;### (autoloads nil "erc-capab" "erc-capab.el" (23323 26117 877880
;;;;;;  399000))
;;; Generated autoloads from erc-capab.el
 (autoload 'erc-capab-identify-mode "erc-capab" nil t)

;;;***

;;;### (autoloads nil "erc-chess" "erc-chess.el" (23323 26118 74546
;;;;;;  493000))
;;; Generated autoloads from erc-chess.el

(defvar erc-ctcp-query-CHESS-hook '(erc-chess-ctcp-query-handler))

(autoload 'erc-cmd-CHESS "erc-chess" "\
Initiate a chess game via CTCP to NICK.
NICK should be the first and only arg to /chess

\(fn LINE &optional FORCE)" nil nil)

(autoload 'erc-chess-ctcp-query-handler "erc-chess" "\


\(fn PROC NICK LOGIN HOST TO MSG)" nil nil)

;;;***

;;;### (autoloads nil "erc-compat" "erc-compat.el" (23323 26117 837880
;;;;;;  515000))
;;; Generated autoloads from erc-compat.el
 (autoload 'erc-define-minor-mode "erc-compat")

;;;***

;;;### (autoloads nil "erc-dcc" "erc-dcc.el" (23323 26118 174546
;;;;;;  202000))
;;; Generated autoloads from erc-dcc.el
 (autoload 'erc-dcc-mode "erc-dcc")

(autoload 'erc-cmd-DCC "erc-dcc" "\
Parser for /dcc command.
This figures out the dcc subcommand and calls the appropriate routine to
handle it.  The function dispatched should be named \"erc-dcc-do-FOO-command\",
where FOO is one of CLOSE, GET, SEND, LIST, CHAT, etc.

\(fn CMD &rest ARGS)" nil nil)

(autoload 'pcomplete/erc-mode/DCC "erc-dcc" "\
Provides completion for the /DCC command.

\(fn)" nil nil)

(defvar erc-ctcp-query-DCC-hook '(erc-ctcp-query-DCC) "\
Hook variable for CTCP DCC queries")

(autoload 'erc-ctcp-query-DCC "erc-dcc" "\
The function called when a CTCP DCC request is detected by the client.
It examines the DCC subcommand, and calls the appropriate routine for
that subcommand.

\(fn PROC NICK LOGIN HOST TO QUERY)" nil nil)

;;;***

;;;### (autoloads nil "erc-ezbounce" "erc-ezbounce.el" (23323 26117
;;;;;;  871213 751000))
;;; Generated autoloads from erc-ezbounce.el

(autoload 'erc-cmd-ezb "erc-ezbounce" "\
Send EZB commands to the EZBouncer verbatim.

\(fn LINE &optional FORCE)" nil nil)

(autoload 'erc-ezb-get-login "erc-ezbounce" "\
Return an appropriate EZBounce login for SERVER and PORT.
Look up entries in `erc-ezb-login-alist'. If the username or password
in the alist is `nil', prompt for the appropriate values.

\(fn SERVER PORT)" nil nil)

(autoload 'erc-ezb-lookup-action "erc-ezbounce" "\


\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-notice-autodetect "erc-ezbounce" "\
React on an EZBounce NOTICE request.

\(fn PROC PARSED)" nil nil)

(autoload 'erc-ezb-identify "erc-ezbounce" "\
Identify to the EZBouncer server.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-init-session-list "erc-ezbounce" "\
Reset the EZBounce session list to nil.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-end-of-session-list "erc-ezbounce" "\
Indicate the end of the EZBounce session listing.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-add-session "erc-ezbounce" "\
Add an EZBounce session to the session list.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-select "erc-ezbounce" "\
Select an IRC server to use by EZBounce, in ERC style.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-select-session "erc-ezbounce" "\
Select a detached EZBounce session.

\(fn)" nil nil)

(autoload 'erc-ezb-initialize "erc-ezbounce" "\
Add EZBouncer convenience functions to ERC.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "erc-fill" "erc-fill.el" (23323 26118 107879
;;;;;;  730000))
;;; Generated autoloads from erc-fill.el
 (autoload 'erc-fill-mode "erc-fill" nil t)

(autoload 'erc-fill "erc-fill" "\
Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "erc-hecomplete" "erc-hecomplete.el" (23323
;;;;;;  26118 157879 585000))
;;; Generated autoloads from erc-hecomplete.el
 (autoload 'erc-hecomplete-mode "erc-hecomplete" nil t)

;;;***

;;;### (autoloads nil "erc-identd" "erc-identd.el" (23323 26117 731214
;;;;;;  158000))
;;; Generated autoloads from erc-identd.el
 (autoload 'erc-identd-mode "erc-identd")

(autoload 'erc-identd-start "erc-identd" "\
Start an identd server listening to port 8113.
Port 113 (auth) will need to be redirected to port 8113 on your
machine -- using iptables, or a program like redir which can be
run from inetd.  The idea is to provide a simple identd server
when you need one, without having to install one globally on your
system.

\(fn &optional PORT)" t nil)

(autoload 'erc-identd-stop "erc-identd" "\


\(fn &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil "erc-imenu" "erc-imenu.el" (23323 26117 757880
;;;;;;  747000))
;;; Generated autoloads from erc-imenu.el

(autoload 'erc-create-imenu-index "erc-imenu" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "erc-join" "erc-join.el" (23323 26117 931213
;;;;;;  577000))
;;; Generated autoloads from erc-join.el
 (autoload 'erc-autojoin-mode "erc-join" nil t)

;;;***

;;;### (autoloads nil "erc-list" "erc-list.el" (23323 26117 681214
;;;;;;  303000))
;;; Generated autoloads from erc-list.el
 (autoload 'erc-list-mode "erc-list")

;;;***

;;;### (autoloads nil "erc-list-old" "erc-list-old.el" (23323 26118
;;;;;;  44546 581000))
;;; Generated autoloads from erc-list-old.el
 (autoload 'erc-list-old-mode "erc-list-old")

(autoload 'erc-list-channels "erc-list-old" "\
Display a buffer containing a list of channels on the current server.
Optional argument CHANNEL specifies a single channel to list (instead of every
available channel).

\(fn &rest CHANNEL)" t nil)

(autoload 'erc-chanlist "erc-list-old" "\
Show a channel listing of the current server in a special mode.
Please note that this function only works with IRC servers which conform
to RFC and send the LIST header (#321) at start of list transmission.

\(fn &optional CHANNELS)" t nil)

;;;***

;;;### (autoloads nil "erc-log" "erc-log.el" (23323 26118 144546
;;;;;;  290000))
;;; Generated autoloads from erc-log.el
 (autoload 'erc-log-mode "erc-log" nil t)

(autoload 'erc-logging-enabled "erc-log" "\
Return non-nil if logging is enabled for BUFFER.
If BUFFER is nil, the value of `current-buffer' is used.
Logging is enabled if `erc-log-channels-directory' is non-nil, the directory
is writeable (it will be created as necessary) and
`erc-enable-logging' returns a non-nil value.

\(fn &optional BUFFER)" nil nil)

(autoload 'erc-save-buffer-in-logs "erc-log" "\
Append BUFFER contents to the log file, if logging is enabled.
If BUFFER is not provided, current buffer is used.
Logging is enabled if `erc-logging-enabled' returns non-nil.

This is normally done on exit, to save the unsaved portion of the
buffer, since only the text that runs off the buffer limit is logged
automatically.

You can save every individual message by putting this function on
`erc-insert-post-hook'.

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads nil "erc-match" "erc-match.el" (23323 26117 737880
;;;;;;  805000))
;;; Generated autoloads from erc-match.el
 (autoload 'erc-match-mode "erc-match")

(autoload 'erc-add-pal "erc-match" "\
Add pal interactively to `erc-pals'.

\(fn)" t nil)

(autoload 'erc-delete-pal "erc-match" "\
Delete pal interactively to `erc-pals'.

\(fn)" t nil)

(autoload 'erc-add-fool "erc-match" "\
Add fool interactively to `erc-fools'.

\(fn)" t nil)

(autoload 'erc-delete-fool "erc-match" "\
Delete fool interactively to `erc-fools'.

\(fn)" t nil)

(autoload 'erc-add-keyword "erc-match" "\
Add keyword interactively to `erc-keywords'.

\(fn)" t nil)

(autoload 'erc-delete-keyword "erc-match" "\
Delete keyword interactively to `erc-keywords'.

\(fn)" t nil)

(autoload 'erc-add-dangerous-host "erc-match" "\
Add dangerous-host interactively to `erc-dangerous-hosts'.

\(fn)" t nil)

(autoload 'erc-delete-dangerous-host "erc-match" "\
Delete dangerous-host interactively to `erc-dangerous-hosts'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "erc-menu" "erc-menu.el" (23323 26117 887880
;;;;;;  369000))
;;; Generated autoloads from erc-menu.el
 (autoload 'erc-menu-mode "erc-menu" nil t)

;;;***

;;;### (autoloads nil "erc-netsplit" "erc-netsplit.el" (23323 26117
;;;;;;  904546 988000))
;;; Generated autoloads from erc-netsplit.el
 (autoload 'erc-netsplit-mode "erc-netsplit")

(autoload 'erc-cmd-WHOLEFT "erc-netsplit" "\
Show who's gone.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "erc-networks" "erc-networks.el" (23323 26117
;;;;;;  944546 871000))
;;; Generated autoloads from erc-networks.el

(autoload 'erc-determine-network "erc-networks" "\
Return the name of the network or \"Unknown\" as a symbol.  Use the
server parameter NETWORK if provided, otherwise parse the server name and
search for a match in `erc-networks-alist'.

\(fn)" nil nil)

(autoload 'erc-server-select "erc-networks" "\
Interactively select a server to connect to using `erc-server-alist'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "erc-notify" "erc-notify.el" (23323 26117 911213
;;;;;;  635000))
;;; Generated autoloads from erc-notify.el
 (autoload 'erc-notify-mode "erc-notify" nil t)

(autoload 'erc-cmd-NOTIFY "erc-notify" "\
Change `erc-notify-list' or list current notify-list members online.
Without args, list the current list of notificated people online,
with args, toggle notify status of people.

\(fn &rest ARGS)" nil nil)

(autoload 'pcomplete/erc-mode/NOTIFY "erc-notify" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "erc-page" "erc-page.el" (23323 26117 707880
;;;;;;  892000))
;;; Generated autoloads from erc-page.el
 (autoload 'erc-page-mode "erc-page")

;;;***

;;;### (autoloads nil "erc-pcomplete" "erc-pcomplete.el" (23323 26117
;;;;;;  714547 540000))
;;; Generated autoloads from erc-pcomplete.el
 (autoload 'erc-completion-mode "erc-pcomplete" nil t)

;;;***

;;;### (autoloads nil "erc-replace" "erc-replace.el" (23323 26117
;;;;;;  921213 606000))
;;; Generated autoloads from erc-replace.el
 (autoload 'erc-replace-mode "erc-replace")

;;;***

;;;### (autoloads nil "erc-ring" "erc-ring.el" (23323 26118 91213
;;;;;;  112000))
;;; Generated autoloads from erc-ring.el
 (autoload 'erc-ring-mode "erc-ring" nil t)

;;;***

;;;### (autoloads nil "erc-services" "erc-services.el" (23323 26117
;;;;;;  771214 42000))
;;; Generated autoloads from erc-services.el
 (autoload 'erc-services-mode "erc-services" nil t)

(autoload 'erc-nickserv-identify-mode "erc-services" "\
Set up hooks according to which MODE the user has chosen.

\(fn MODE)" t nil)

(autoload 'erc-nickserv-identify "erc-services" "\
Send an \"identify <PASSWORD>\" message to NickServ.
When called interactively, read the password using `read-passwd'.

\(fn PASSWORD)" t nil)

;;;***

;;;### (autoloads nil "erc-sound" "erc-sound.el" (23323 26117 977880
;;;;;;  108000))
;;; Generated autoloads from erc-sound.el
 (autoload 'erc-sound-mode "erc-sound")

;;;***

;;;### (autoloads nil "erc-speedbar" "erc-speedbar.el" (23323 26117
;;;;;;  994546 726000))
;;; Generated autoloads from erc-speedbar.el

(autoload 'erc-speedbar-browser "erc-speedbar" "\
Initialize speedbar to display an ERC browser.
This will add a speedbar major display mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "erc-spelling" "erc-spelling.el" (23323 26117
;;;;;;  764547 394000))
;;; Generated autoloads from erc-spelling.el
 (autoload 'erc-spelling-mode "erc-spelling" nil t)

;;;***

;;;### (autoloads nil "erc-stamp" "erc-stamp.el" (23323 26118 57879
;;;;;;  875000))
;;; Generated autoloads from erc-stamp.el
 (autoload 'erc-timestamp-mode "erc-stamp" nil t)

;;;***

;;;### (autoloads nil "erc-track" "erc-track.el" (23323 26117 697880
;;;;;;  922000))
;;; Generated autoloads from erc-track.el

(defvar erc-track-minor-mode nil "\
Non-nil if Erc-Track minor mode is enabled.
See the `erc-track-minor-mode' command
for a description of this minor mode.")

(custom-autoload 'erc-track-minor-mode "erc-track" nil)

(autoload 'erc-track-minor-mode "erc-track" "\
Global minor mode for tracking ERC buffers and showing activity in the
mode line.

This exists for the sole purpose of providing the C-c C-SPC and
C-c C-@ keybindings.  Make sure that you have enabled the track
module, otherwise the keybindings will not do anything useful.

\(fn &optional ARG)" t nil)
 (autoload 'erc-track-mode "erc-track" nil t)

;;;***

;;;### (autoloads nil "erc-truncate" "erc-truncate.el" (23323 26117
;;;;;;  894547 16000))
;;; Generated autoloads from erc-truncate.el
 (autoload 'erc-truncate-mode "erc-truncate" nil t)

(autoload 'erc-truncate-buffer-to-size "erc-truncate" "\
Truncates the buffer to the size SIZE.
If BUFFER is not provided, the current buffer is assumed.  The deleted
region is logged if `erc-logging-enabled' returns non-nil.

\(fn SIZE &optional BUFFER)" nil nil)

(autoload 'erc-truncate-buffer "erc-truncate" "\
Truncates the current buffer to `erc-max-buffer-size'.
Meant to be used in hooks, like `erc-insert-post-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "erc-xdcc" "erc-xdcc.el" (23323 26118 11213
;;;;;;  345000))
;;; Generated autoloads from erc-xdcc.el
 (autoload 'erc-xdcc-mode "erc-xdcc")

(autoload 'erc-xdcc-add-file "erc-xdcc" "\
Add a file to `erc-xdcc-files'.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil nil ("erc-backend.el" "erc-goodies.el" "erc-ibuffer.el"
;;;;;;  "erc-lang.el" "erc-maint.el" "erc-nicklist.el" "erc-pkg.el"
;;;;;;  "erc-speak.el" "erc-viper.el") (23323 26118 27879 962000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; erc-autoloads.el ends here
