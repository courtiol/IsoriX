"""Generate the IsoriX hex logo as SVG.

Usage:
  python3 make_logo.py [FONT_FILE] [OUT.svg] [SIZE] [TRACKING] [PIN_X] [PIN_Y]
Defaults: P052 Bold (URW Palatino clone), logo.svg, 108, 0, 360, 300.
Requires: numpy, matplotlib (filled contours of the isoscape), fonttools
(text is converted to outlines so the SVG renders without the font).
PNGs were rendered from the SVG with headless Chromium.

Elements: an isoscape (filled contours of a 2D field coloured with
isopalette1), a bat flying to the assigned origin (pin inside a closed
contour), and a deuterium atom (1 proton + 1 neutron, 1 electron) labelled 2H.
"""
import sys, math
import numpy as np
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from fontTools.ttLib import TTFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen

FONT = sys.argv[1] if len(sys.argv) > 1 else "/usr/share/fonts/opentype/urw-base35/P052-Bold.otf"
OUT = sys.argv[2] if len(sys.argv) > 2 else "logo2.svg"
SIZE = float(sys.argv[3]) if len(sys.argv) > 3 else 108
TRACK = float(sys.argv[4]) if len(sys.argv) > 4 else 0

PX, PY = float(sys.argv[5]) if len(sys.argv) > 5 else 360, float(sys.argv[6]) if len(sys.argv) > 6 else 300
# IsoriX::isopalette1
pal = ['#006198', '#006499', '#00679A', '#006A9C', '#006D9E', '#0071A0', '#0075A2', '#0079A5', '#007DA7', '#0081AA', '#0086AD', '#008AB0', '#008FB3', '#0094B7', '#3099BA', '#479FBE', '#59A5C1', '#6AABC5', '#79B1C9', '#88B8CE', '#97BFD2', '#A6C7D7', '#B6CFDC', '#C8D9E2', '#DEE6EA', '#EBE3E0', '#E5D3CC', '#E0C6BC', '#DBBCAE', '#D6B2A1', '#D1A895', '#CDA08A', '#C8977F', '#C49074', '#C0886A', '#BB815F', '#B77A55', '#B3734A', '#AF6D3F', '#AB6633', '#A66025', '#A25A11', '#9E5400', '#9A4E00', '#964900', '#924300', '#8D3D00', '#893800', '#853300', '#812D00']
W, H = 518, 600
hexpts = [(259,0),(518,150),(518,450),(259,600),(0,450),(0,150)]
def hexpath(pts): return "M" + " L".join(f"{x:.1f},{y:.1f}" for x,y in pts) + " Z"

# ---- isoscape: a 2D field (trend + relief + noise), filled contours ----
x = np.arange(-20, W+21, 2.0); y = np.arange(-20, H+21, 2.0)
X, Y = np.meshgrid(x, y)
def g(cx, cy, sx, sy, a, th=0):
    c, s = math.cos(th), math.sin(th)
    u = (X-cx)*c + (Y-cy)*s; v = -(X-cx)*s + (Y-cy)*c
    return a*np.exp(-0.5*((u/sx)**2 + (v/sy)**2))
F = (Y/600)*1.0                                   # latitudinal trend
F += 0.035*np.sin(X/70 + 0.8) + 0.03*np.sin(X/37 + Y/53 + 2.0) + 0.02*np.sin(Y/31 - X/45)
F += g(375, 300, 100, 42, -0.21, -0.35)            # "mountain range": depleted tongue
F += g(120, 345, 60, 45, -0.10)                   # second depleted patch
F += g(300, 175, 70, 30, 0.07, 0.4)
F += g(150, 520, 80, 40, 0.06)
F += g(70, 385, 95, 32, -0.09, 0.25)
F += g(470, 395, 60, 45, 0.07)
F += g(250, 345, 55, 30, 0.05, -0.2)
F += g(259, 560, 340, 115, 0.30)
levels = np.quantile(F, np.linspace(0, 1, 15)); levels[0] -= 1e-6; levels[-1] += 1e-6
ncol = len(levels)-1
cols = [pal[round(i*(len(pal)-1)/(ncol-1))] for i in range(ncol)]
fig, ax = plt.subplots()
cs = ax.contourf(X, Y, F, levels=levels)
bands = []
for i, path in enumerate(cs.get_paths()):
    d = ""
    for poly in path.to_polygons(closed_only=True):
        if len(poly) < 3: continue
        d += "M" + " L".join(f"{a:.1f},{b:.1f}" for a, b in poly) + "Z"
    if d: bands.append(f'<path d="{d}" fill="{cols[i]}" fill-rule="evenodd"/>')
cl = ax.contour(X, Y, F, levels=levels[1:-1])
lines = []
for path in cl.get_paths():
    d = ""
    for poly in path.to_polygons(closed_only=False):
        if len(poly) < 2: continue
        d += "M" + " L".join(f"{a:.1f},{b:.1f}" for a, b in poly)
    if d: lines.append(d)
plt.close(fig)
lines_svg = f'<path d="{" ".join(lines)}" fill="none" stroke="#ffffff" stroke-opacity="0.38" stroke-width="1.4" stroke-linejoin="round"/>'

# ---- bat ----
R = [(0,-11),(3.5,-22),(7,-10),("Q",(10,-5),(11,-2)),
     ("Q",(30,-12),(48,-34)),("Q",(68,-32),(88,-26)),
     ("Q",(78,-14),(77,0)),("Q",(67,-6),(58,8)),("Q",(47,2),(38,14)),("Q",(27,6),(15,13)),
     ("Q",(11,20),(6,25)),("Q",(3,27),(0,28))]
def batpath():
    d = f"M{R[0][0]},{R[0][1]}"
    for s in R[1:]:
        d += f" Q{s[1][0]},{s[1][1]} {s[2][0]},{s[2][1]}" if s[0]=="Q" else f" L{s[0]},{s[1]}"
    ends = [s[2] if s[0]=="Q" else s for s in R]
    for k in range(len(R)-1, 0, -1):
        s = R[k]; pe = ends[k-1]
        d += f" Q{-s[1][0]},{s[1][1]} {-pe[0]},{pe[1]}" if s[0]=="Q" else f" L{-pe[0]},{pe[1]}"
    return d + " Z"
bx, by, bs, rot = 300, 100, 1.1, -4
px, py = PX, PY
traj = f'M{bx+4},{by+36} C{bx+10},{by+110} {px-70},{py-130} {px-4},{py-86}'

pin = (f'<g transform="translate({px},{py})">'
       f'<ellipse cx="0" cy="2" rx="26" ry="8" fill="none" stroke="#ffffff" stroke-width="3" stroke-opacity="0.9"/>'
       f'<path d="M0,0 C-6,-16 -24,-30 -24,-48 A24,24 0 1 1 24,-48 C24,-30 6,-16 0,0 Z" fill="#c8102e" stroke="#ffffff" stroke-width="4" stroke-linejoin="round"/>'
       f'<circle cx="0" cy="-48" r="9" fill="#ffffff"/></g>')

# ---- isotope: deuterium atom (1 proton + 1 neutron, 1 electron) ----
ax_, ay_ = 128, 168
atom = (f'<g transform="translate({ax_},{ay_})">'
        f'<ellipse cx="0" cy="0" rx="62" ry="22" transform="rotate(-28)" fill="none" stroke="#ffffff" stroke-width="3.2"/>'
        f'<ellipse cx="0" cy="0" rx="58" ry="21" transform="rotate(32)" fill="none" stroke="#ffffff" stroke-width="3" stroke-opacity="0"/>'
        f'<circle cx="-10" cy="2" r="14" fill="#f4f1ec" stroke="#10212b" stroke-width="2"/>'
        f'<circle cx="10" cy="-2" r="14" fill="#c8102e" stroke="#ffffff" stroke-width="2.5"/>'
        f'<circle cx="{62*math.cos(math.radians(-28))*math.cos(2.2) - 22*math.sin(math.radians(-28))*math.sin(2.2):.1f}" '
        f'cy="{62*math.sin(math.radians(-28))*math.cos(2.2) + 22*math.cos(math.radians(-28))*math.sin(2.2):.1f}" r="7" fill="#ffffff" stroke="#10212b" stroke-width="2"/>'
        f'</g>')

# ---- name, outlined ----
def textpath(fontfile, txt, size, cx, base, track=0, anchor="middle"):
    f = TTFont(fontfile); gs = f.getGlyphSet(); cmap = f.getBestCmap(); sc = size/f["head"].unitsPerEm
    nm = [cmap[ord(c)] for c in txt]; adv = [gs[n].width*sc for n in nm]
    tx = cx - (sum(adv) + track*(len(nm)-1))/2 if anchor == "middle" else cx
    pen = SVGPathPen(gs)
    for n, a in zip(nm, adv):
        gs[n].draw(TransformPen(pen, (sc, 0, 0, -sc, tx, base))); tx += a + track
    return pen.getCommands()
label = f'<path d="{textpath(FONT, "\u00b2H", 40, ax_+2, ay_+74)}" fill="#ffffff" filter="url(#sh)"/>'

f = TTFont(FONT); gs = f.getGlyphSet(); cmap = f.getBestCmap(); sc = SIZE/f["head"].unitsPerEm
names = [cmap[ord(c)] for c in "IsoriX"]; adv = [gs[n].width*sc for n in names]
tx = 259 - (sum(adv) + TRACK*(len(names)-1))/2; base = 482
pen = SVGPathPen(gs)
for n, a in zip(names, adv):
    gs[n].draw(TransformPen(pen, (sc, 0, 0, -sc, tx, base))); tx += a + TRACK
name = f'<path d="{pen.getCommands()}" fill="#ffffff" filter="url(#sh)"/>'

svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
<defs><clipPath id="hex"><path d="{hexpath(hexpts)}"/></clipPath>
<filter id="sh" x="-20%" y="-20%" width="140%" height="140%"><feDropShadow dx="0" dy="3" stdDeviation="3" flood-color="#000" flood-opacity="0.35"/></filter></defs>
<g clip-path="url(#hex)">
{chr(10).join(bands)}
{lines_svg}
<path d="{traj}" fill="none" stroke="#ffffff" stroke-width="4" stroke-dasharray="2 10" stroke-linecap="round"/>
<g filter="url(#sh)">{pin}</g>
<g filter="url(#sh)">{atom}</g>
<g transform="translate({bx},{by}) rotate({rot}) scale({bs})" filter="url(#sh)"><path d="{batpath()}" fill="#10212b" stroke="#ffffff" stroke-width="2.4" stroke-linejoin="round"/></g>
{label}
{name}
</g>
<path d="{hexpath(hexpts)}" fill="none" stroke="#0a2f45" stroke-width="20" clip-path="url(#hex)"/>
</svg>'''
open(OUT, "w").write(svg)
