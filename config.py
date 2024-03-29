
import re
from xkeysnail.transform import *

# [Global modemap] Change modifier keys as in xmodmap
define_modmap({
    Key.CAPSLOCK: Key.LEFT_CTRL,
    Key.KATAKANAHIRAGANA: Key.RIGHT_META,
    Key.HENKAN: Key.MUHENKAN,
    Key.LEFT_ALT: Key.LEFT_META,
    Key.LEFT_META: Key.LEFT_ALT
})

define_keymap(lambda wm_class: ((wm_class not in ("Emacs", "Gnome-terminal", "gnome-terminal-server", "jetbrains-idea", "term1", "term2", "termL", "termR", "mainterm", "fzf_actions", "intellij-terminal", "eDEX-UI")) and (not wm_class.startswith("xmonad.intellij")) and (not wm_class.startswith("xmonad.terminal"))), {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-Shift-b"): with_mark(K("Shift-left")),
    K("C-Shift-f"): with_mark(K("Shift-right")),
    K("C-p"): with_mark(K("up")),
    K("C-n"): with_mark(K("down")),
    K("C-Shift-p"): with_mark(K("Shift-up")),
    K("C-Shift-n"): with_mark(K("Shift-down")),
    K("C-h"): with_mark(K("backspace")),
    # Forward/Backward word
    K("M-b"): with_mark(K("C-left")),
    K("M-f"): with_mark(K("C-right")),
    K("M-Shift-b"): with_mark(K("C-Shift-left")),
    K("M-Shift-f"): with_mark(K("C-Shift-right")),
    # Beginning/End of line
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    K("C-Shift-a"): with_mark(K("Shift-home")),
    K("C-Shift-e"): with_mark(K("Shift-end")),
    # Page up/down
    K("M-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    # Beginning/End of file
    K("M-Shift-comma"): with_mark(K("C-home")),
    K("M-Shift-dot"): with_mark(K("C-end")),
    # Newline
#    K("C-j"): K("enter"),
#    K("C-o"): [K("enter"), K("left")],
    # Copy
    K("C-w"): [K("C-x"), set_mark(False)],
    K("M-w"): [K("C-c"), set_mark(False)],
    K("C-y"): [K("C-v"), set_mark(False)],
    # Delete
    K("C-d"): [K("delete"), set_mark(False)],
    K("M-d"): [K("C-delete"), set_mark(False)],
    # Kill line
    K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
    # Undo
    K("C-slash"): [K("C-z"), set_mark(False)],
    K("C-Shift-ro"): K("C-z"),
    # Mark
    K("C-m"): set_mark(True),
#   K("C-space"): set_mark(True),
#    K("C-M-space"): with_or_set_mark(K("C-right")),
    # Search
    K("C-s"): K("F3"),
#    K("C-r"): K("Shift-F3"),
    K("M-Shift-key_5"): K("C-h"),
    # Cancel
    K("C-g"): [K("esc"), set_mark(False)],
    # Escape
    K("C-q"): escape_next_key,
    # C-x YYY
    K("C-x"): {
        # C-x h (select all)
        K("h"): [K("C-home"), K("C-a"), set_mark(True)],
        K("C-a"): [K("C-a")],
        K("C-x"): [K("C-x"), set_mark(False)],
        # C-x C-f (open)
        K("C-f"): K("C-o"),
        # C-x C-s (save)
        K("C-s"): K("C-s"),
        # C-x k (kill tab)
        K("k"): K("C-f4"),
        # C-x C-c (exit)
        K("C-c"): K("C-q"),
        # cancel
        K("C-g"): pass_through_key,
        # C-x u (undo)
        K("u"): [K("C-z"), set_mark(False)],
    }
}, "Emacs-like keys")


define_keymap(lambda wm_class: wm_class in ("jetbrains-idea"), {
    # Cancel
    K("C-g"): [K("esc"), set_mark(False)],
    K("M-n"): K("M-tab"),
    K("M-p"): K("M-Shift-tab")
}, "jetbrains-idea keys")

