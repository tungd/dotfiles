"""Compact, left-aligned Kitty tab bar."""

from kitty.tab_bar import draw_title


def draw_tab(draw_data, screen, tab, before, max_tab_length, index, is_last, extra_data):
    """Draw a compact tab, letting background shades separate neighboring tabs."""
    # Kitty's horizontal tab bar is one cell high, so the clean active-state
    # signal is the theme's active background plus the configured bold font.
    title_width = max(1, max_tab_length - 2)

    screen.draw(" ")
    draw_title(draw_data, screen, tab, index, title_width)
    screen.draw(" ")

    return screen.cursor.x
