#!/usr/bin/env python3
"""Convert TRISK Installation Guide markdown to professional PDF using fpdf2."""

import re
import sys
import os
from fpdf import FPDF

# ── Configuration ──
MARGIN_LEFT = 25
MARGIN_RIGHT = 25
MARGIN_TOP = 25
MARGIN_BOTTOM = 30
PAGE_W = 210  # A4
CONTENT_W = PAGE_W - MARGIN_LEFT - MARGIN_RIGHT

# Colors (R, G, B)
BLACK = (26, 26, 26)
DARK = (51, 51, 51)
MID = (100, 100, 100)
LIGHT_GRAY = (200, 200, 200)
BG_GRAY = (245, 245, 245)
TABLE_HEADER_BG = (44, 44, 44)
TABLE_HEADER_FG = (255, 255, 255)
TABLE_EVEN_BG = (247, 247, 247)
TABLE_ODD_BG = (255, 255, 255)
CODE_BG = (240, 240, 240)
CODE_BORDER = (200, 200, 200)


class InstallGuidePDF(FPDF):
    def __init__(self):
        super().__init__("P", "mm", "A4")
        self.set_auto_page_break(True, MARGIN_BOTTOM)
        self.set_margins(MARGIN_LEFT, MARGIN_TOP, MARGIN_RIGHT)
        self.is_first_page = True

    def header(self):
        if self.page_no() == 1:
            return
        self.set_font("Helvetica", "I", 7)
        self.set_text_color(160, 160, 160)
        self.set_y(10)
        self.cell(0, 5, "TRISK Desktop - Installation Guide v1.0", align="R")

    def footer(self):
        self.set_y(-20)
        self.set_font("Helvetica", "", 7)
        self.set_text_color(160, 160, 160)
        self.cell(0, 5, f"{self.page_no()}", align="C", new_x="LMARGIN", new_y="NEXT")
        if self.page_no() > 1:
            self.cell(0, 5, "Internal - For IT / InfoSec / Risk Technology", align="R")


def sanitize_unicode(text):
    """Replace Unicode chars with ASCII equivalents for latin-1 fonts."""
    replacements = {
        '\u2014': ' - ',   # em dash
        '\u2013': '-',     # en dash
        '\u2018': "'",     # left single quote
        '\u2019': "'",     # right single quote
        '\u201c': '"',     # left double quote
        '\u201d': '"',     # right double quote
        '\u2026': '...',   # ellipsis
        '\u2192': '->',    # right arrow
        '\u2605': '*',     # star
        '\u2022': '-',     # bullet
        '\u2713': 'Y',     # check mark
        '\u2717': 'N',     # cross mark
        '\u2610': '[ ]',   # ballot box
        '\u2611': '[x]',   # ballot box checked
        '\u2612': '[x]',   # ballot box with x
        '\u00a0': ' ',     # non-breaking space
        '\u2500': '-',     # box drawing horizontal
        '\u2502': '|',     # box drawing vertical
        '\u250c': '+',     # box drawing corner
        '\u2510': '+',
        '\u2514': '+',
        '\u2518': '+',
        '\u251c': '+',
        '\u2524': '+',
        '\u252c': '+',
        '\u2534': '+',
        '\u253c': '+',
        '\u2550': '=',     # double horizontal
        '\u2551': '|',     # double vertical
        '\u2190': '<-',    # left arrow
        '\u2191': '^',     # up arrow
        '\u2193': 'v',     # down arrow
        '\u23f0': '',      # alarm clock emoji
        '\u2705': '[OK]',  # check mark
        '\u2b50': '*',     # star
    }
    for k, v in replacements.items():
        text = text.replace(k, v)
    # Fallback: replace any remaining non-latin-1 chars
    text = text.encode('latin-1', errors='replace').decode('latin-1')
    return text


def clean(text):
    """Strip markdown inline formatting for plain text output."""
    text = re.sub(r'\*\*(.+?)\*\*', r'\1', text)
    text = re.sub(r'\*(.+?)\*', r'\1', text)
    text = re.sub(r'`(.+?)`', r'\1', text)
    text = re.sub(r'\[(.+?)\]\(.+?\)', r'\1', text)
    return sanitize_unicode(text.strip())


def has_bold(text):
    """Check if line contains bold markers."""
    return '**' in text


def render_rich_line(pdf, text, base_size=9, base_style=""):
    """Render a line with mixed bold/code/normal formatting using write()."""
    # Parse into segments: bold, code, normal
    pattern = r'(\*\*.*?\*\*|`[^`]+`|\[[^\]]+\]\([^\)]+\))'
    parts = re.split(pattern, text)

    for part in parts:
        if part.startswith('**') and part.endswith('**'):
            pdf.set_font("Helvetica", "B", base_size)
            pdf.set_text_color(*BLACK)
            pdf.write(5, sanitize_unicode(part[2:-2]))
        elif part.startswith('`') and part.endswith('`'):
            pdf.set_font("Courier", "", base_size - 1)
            pdf.set_text_color(180, 50, 70)
            pdf.write(5, sanitize_unicode(part[1:-1]))
        elif part.startswith('[') and '](' in part:
            m = re.match(r'\[(.+?)\]\((.+?)\)', part)
            if m:
                pdf.set_font("Helvetica", "", base_size)
                pdf.set_text_color(*DARK)
                pdf.write(5, sanitize_unicode(m.group(1)))
        else:
            pdf.set_font("Helvetica", base_style, base_size)
            pdf.set_text_color(*BLACK)
            pdf.write(5, sanitize_unicode(part))

    pdf.ln(5.5)


def build_pdf(md_path, output_path):
    with open(md_path, "r") as f:
        lines = f.readlines()

    pdf = InstallGuidePDF()
    pdf.add_page()

    i = 0
    in_code_block = False
    code_lines = []
    in_table = False
    table_rows = []

    while i < len(lines):
        line = lines[i].rstrip("\n")

        # ── Code blocks ──
        if line.startswith("```"):
            if in_code_block:
                # End code block — render it
                pdf.set_font("Courier", "", 7.5)
                pdf.set_fill_color(*CODE_BG)
                pdf.set_draw_color(*CODE_BORDER)
                pdf.set_text_color(*DARK)

                block_text = "\n".join(code_lines)
                # Calculate height
                num_lines = len(code_lines)
                block_h = max(num_lines * 4 + 6, 10)

                # Check if we need a page break
                if pdf.get_y() + block_h > 270:
                    pdf.add_page()

                x = pdf.get_x()
                y = pdf.get_y()
                pdf.rect(x, y, CONTENT_W, block_h, style="DF")
                # Left accent bar
                pdf.set_fill_color(140, 140, 140)
                pdf.rect(x, y, 1.5, block_h, style="F")
                pdf.set_fill_color(*CODE_BG)

                pdf.set_xy(x + 4, y + 3)
                # Estimate max chars that fit in code block width
                # Courier at 7.5pt ~ 1.9mm per char
                max_chars = int((CONTENT_W - 8) / 1.9)
                for cl in code_lines:
                    pdf.set_x(x + 4)
                    truncated = sanitize_unicode(cl[:max_chars])
                    pdf.cell(CONTENT_W - 8, 4, truncated, new_x="LMARGIN", new_y="NEXT")

                pdf.set_y(y + block_h + 3)
                code_lines = []
                in_code_block = False
            else:
                in_code_block = True
                code_lines = []
            i += 1
            continue

        if in_code_block:
            code_lines.append(line)
            i += 1
            continue

        # ── Tables ──
        if "|" in line and line.strip().startswith("|"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            # Skip separator rows
            if all(re.match(r'^[-:]+$', c) for c in cells):
                i += 1
                continue
            if not in_table:
                in_table = True
                table_rows = []
            table_rows.append(cells)
            # Look ahead — is next line still a table?
            if i + 1 < len(lines):
                next_l = lines[i + 1].strip()
                if not (next_l.startswith("|") and "|" in next_l[1:]):
                    # Render table
                    render_table(pdf, table_rows)
                    in_table = False
                    table_rows = []
            else:
                render_table(pdf, table_rows)
                in_table = False
                table_rows = []
            i += 1
            continue

        # ── Empty lines ──
        if line.strip() == "":
            pdf.ln(2)
            i += 1
            continue

        # ── Horizontal rule ──
        if line.strip().startswith("---"):
            pdf.ln(4)
            pdf.set_draw_color(*LIGHT_GRAY)
            pdf.line(MARGIN_LEFT, pdf.get_y(), PAGE_W - MARGIN_RIGHT, pdf.get_y())
            pdf.ln(4)
            i += 1
            continue

        # ── Headings ──
        if line.startswith("# ") and not line.startswith("## "):
            text = line[2:].strip()
            if pdf.page_no() == 1 and pdf.get_y() < 50:
                # Title page
                pdf.ln(30)
                pdf.set_font("Helvetica", "B", 22)
                pdf.set_text_color(*BLACK)
                pdf.multi_cell(CONTENT_W, 10, clean(text), align="L")
                pdf.set_draw_color(50, 50, 50)
                pdf.line(MARGIN_LEFT, pdf.get_y() + 3, PAGE_W - MARGIN_RIGHT, pdf.get_y() + 3)
                pdf.ln(6)
            else:
                if pdf.get_y() > 60:
                    pdf.add_page()
                pdf.ln(6)
                pdf.set_font("Helvetica", "B", 16)
                pdf.set_text_color(*BLACK)
                pdf.multi_cell(CONTENT_W, 8, clean(text), align="L")
                pdf.set_draw_color(200, 200, 200)
                pdf.line(MARGIN_LEFT, pdf.get_y() + 1, PAGE_W - MARGIN_RIGHT, pdf.get_y() + 1)
                pdf.ln(4)
            i += 1
            continue

        if line.startswith("## "):
            text = line[3:].strip()
            if pdf.get_y() > 230:
                pdf.add_page()
            pdf.ln(6)
            pdf.set_font("Helvetica", "B", 13)
            pdf.set_text_color(30, 30, 30)
            pdf.multi_cell(CONTENT_W, 7, clean(text), align="L")
            pdf.set_draw_color(220, 220, 220)
            pdf.line(MARGIN_LEFT, pdf.get_y() + 1, MARGIN_LEFT + 80, pdf.get_y() + 1)
            pdf.ln(4)
            i += 1
            continue

        if line.startswith("### "):
            text = line[4:].strip()
            if pdf.get_y() > 250:
                pdf.add_page()
            pdf.ln(4)
            pdf.set_font("Helvetica", "B", 11)
            pdf.set_text_color(*DARK)
            pdf.multi_cell(CONTENT_W, 6, clean(text), align="L")
            pdf.ln(2)
            i += 1
            continue

        if line.startswith("#### "):
            text = line[5:].strip()
            pdf.ln(3)
            pdf.set_font("Helvetica", "B", 10)
            pdf.set_text_color(*DARK)
            pdf.multi_cell(CONTENT_W, 5.5, clean(text), align="L")
            pdf.ln(2)
            i += 1
            continue

        # ── Blockquotes ──
        if line.startswith("> "):
            text = line[2:].strip()
            pdf.set_font("Helvetica", "I", 8.5)
            pdf.set_text_color(100, 100, 100)
            pdf.set_fill_color(250, 250, 250)
            x = pdf.get_x()
            y = pdf.get_y()
            pdf.set_x(x + 5)
            pdf.multi_cell(CONTENT_W - 10, 5, clean(text), fill=True)
            # Left bar
            pdf.set_fill_color(180, 180, 180)
            pdf.rect(x, y, 1.5, pdf.get_y() - y, style="F")
            pdf.ln(2)
            i += 1
            continue

        # ── Checkbox list items ──
        if line.strip().startswith("- [ ]") or line.strip().startswith("- [x]"):
            checked = "[x]" in line
            text = re.sub(r'^-\s*\[.\]\s*', '', line.strip())
            marker = "[x]" if checked else "[ ]"
            pdf.set_font("Helvetica", "", 9)
            pdf.set_text_color(*BLACK)
            indent = (len(line) - len(line.lstrip())) * 1.5 + 3
            pdf.set_x(MARGIN_LEFT + indent)
            pdf.cell(5, 5, marker)
            pdf.set_x(MARGIN_LEFT + indent + 5)
            # Use multi_cell for wrapping
            pdf.multi_cell(CONTENT_W - indent - 5, 5, clean(text))
            i += 1
            continue

        # ── List items ──
        if line.strip().startswith("- ") or re.match(r'^\s*\d+\.\s', line.strip()):
            text = re.sub(r'^\s*[-\d.]+\s*', '', line.strip())
            indent = (len(line) - len(line.lstrip())) * 1.5 + 3
            pdf.set_x(MARGIN_LEFT + indent)
            pdf.set_font("Helvetica", "", 9)
            pdf.set_text_color(*BLACK)
            pdf.cell(4, 5, "-")
            pdf.set_x(MARGIN_LEFT + indent + 4)
            pdf.multi_cell(CONTENT_W - indent - 4, 5, clean(text))
            i += 1
            continue

        # ── Bold metadata lines (Version:, Classification:, etc.) ──
        if line.startswith("**") and ":**" in line:
            pdf.set_font("Helvetica", "", 9)
            pdf.set_text_color(*DARK)
            render_rich_line(pdf, line, base_size=9)
            i += 1
            continue

        # ── Normal paragraph text ──
        pdf.set_font("Helvetica", "", 9)
        pdf.set_text_color(*BLACK)
        render_rich_line(pdf, line, base_size=9)
        i += 1

    pdf.output(output_path)
    print(f"PDF saved: {output_path}")
    print(f"Pages: {pdf.page_no()}")


def render_table(pdf, rows):
    """Render a table with proper column widths and styling."""
    if not rows:
        return

    n_cols = len(rows[0])
    if n_cols == 0:
        return

    # Calculate column widths based on content + header lengths
    col_widths = []
    for c in range(n_cols):
        # Use header length as minimum baseline
        header_len = len(clean(rows[0][c])) if c < len(rows[0]) else 5
        max_len = header_len
        for r in rows[1:]:
            if c < len(r):
                max_len = max(max_len, len(clean(r[c])))
        # Ensure empty columns still get reasonable width from header
        col_widths.append(max(max_len, header_len, 5))

    total = sum(col_widths) or 1
    col_widths = [max(w / total * CONTENT_W, 18) for w in col_widths]

    # Normalize to fit page
    scale = CONTENT_W / sum(col_widths)
    col_widths = [w * scale for w in col_widths]

    # Check if table fits on page
    est_height = len(rows) * 7
    if pdf.get_y() + est_height > 265:
        pdf.add_page()

    # Header row
    pdf.set_font("Helvetica", "B", 7.5)
    pdf.set_fill_color(*TABLE_HEADER_BG)
    pdf.set_text_color(*TABLE_HEADER_FG)
    pdf.set_draw_color(*TABLE_HEADER_BG)

    for c, cell in enumerate(rows[0]):
        if c < len(col_widths):
            # Estimate max chars: Helvetica Bold 7.5pt ~ 2.0mm per char
            max_ch = int(col_widths[c] / 2.0)
            pdf.cell(col_widths[c], 6.5, clean(cell)[:max_ch], border=1, fill=True)
    pdf.ln()

    # Data rows
    pdf.set_font("Helvetica", "", 8)
    pdf.set_draw_color(*LIGHT_GRAY)
    for ri, row in enumerate(rows[1:]):
        if ri % 2 == 0:
            pdf.set_fill_color(*TABLE_EVEN_BG)
        else:
            pdf.set_fill_color(*TABLE_ODD_BG)
        pdf.set_text_color(*BLACK)

        # Calculate row height based on content
        max_lines = 1
        cell_texts = []
        for c in range(n_cols):
            text = clean(row[c]) if c < len(row) else ""
            cell_texts.append(text)
            # Estimate lines needed
            char_w = col_widths[c] / 2.3 if c < len(col_widths) else 30
            lines_needed = max(1, len(text) / max(char_w, 1) + 0.5)
            max_lines = max(max_lines, int(lines_needed))

        row_h = max(5.5, max_lines * 5)

        # Check page break
        if pdf.get_y() + row_h > 270:
            pdf.add_page()
            # Re-render header
            pdf.set_font("Helvetica", "B", 7.5)
            pdf.set_fill_color(*TABLE_HEADER_BG)
            pdf.set_text_color(*TABLE_HEADER_FG)
            pdf.set_draw_color(*TABLE_HEADER_BG)
            for c, cell in enumerate(rows[0]):
                if c < len(col_widths):
                    max_ch = int(col_widths[c] / 2.0)
                    pdf.cell(col_widths[c], 6.5, clean(cell)[:max_ch], border=1, fill=True)
            pdf.ln()
            pdf.set_font("Helvetica", "", 8)
            pdf.set_draw_color(*LIGHT_GRAY)
            if ri % 2 == 0:
                pdf.set_fill_color(*TABLE_EVEN_BG)
            else:
                pdf.set_fill_color(*TABLE_ODD_BG)
            pdf.set_text_color(*BLACK)

        for c, text in enumerate(cell_texts):
            if c < len(col_widths):
                max_ch = int(col_widths[c] / 1.8)
                pdf.cell(col_widths[c], row_h, text[:max_ch], border=1, fill=True)
        pdf.ln()

    pdf.ln(3)


if __name__ == "__main__":
    md_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "TRISK_Desktop_Installation_Guide_v1.md")
    output_path = sys.argv[1] if len(sys.argv) > 1 else "TRISK_Desktop_Installation_Guide_v1.pdf"
    build_pdf(md_path, output_path)
