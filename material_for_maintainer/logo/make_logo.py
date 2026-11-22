"""Generate the IsoriX hex logo (logo.svg).

Usage: python3 make_logo.py

The text uses the font "Inter Display" (Black). In the committed logo.svg the
text was converted to outlines (with fontTools) so that it renders without the
font installed. PNGs were rendered from logo.svg with headless Chromium.
"""
import math, sys
# IsoriX::isopalette1
pal = ['#006198', '#006499', '#00679A', '#006A9C', '#006D9E', '#0071A0', '#0075A2', '#0079A5', '#007DA7', '#0081AA', '#0086AD', '#008AB0', '#008FB3', '#0094B7', '#3099BA', '#479FBE', '#59A5C1', '#6AABC5', '#79B1C9', '#88B8CE', '#97BFD2', '#A6C7D7', '#B6CFDC', '#C8D9E2', '#DEE6EA', '#EBE3E0', '#E5D3CC', '#E0C6BC', '#DBBCAE', '#D6B2A1', '#D1A895', '#CDA08A', '#C8977F', '#C49074', '#C0886A', '#BB815F', '#B77A55', '#B3734A', '#AF6D3F', '#AB6633', '#A66025', '#A25A11', '#9E5400', '#9A4E00', '#964900', '#924300', '#8D3D00', '#893800', '#853300', '#812D00']
W, H = 518, 600
hexpts = [(259,0),(518,150),(518,450),(259,600),(0,450),(0,150)]
inset = 9
def hexpath(pts): return "M" + " L".join(f"{x:.1f},{y:.1f}" for x,y in pts) + " Z"

# isoscape bands: stacked smooth curves, blue (top) -> brown (bottom)
nb = 11
idx = [0, 6, 12, 17, 21, 25, 31, 36, 40, 44, 49]
cols = [pal[i] for i in idx]
def curve(i, x):
    y0 = [0, 52, 102, 150, 196, 240, 292, 340, 394, 456, 530][i]
    return (y0 + 22*math.sin(x/95 + 0.9 + 0.35*i) + 12*math.sin(x/41 + 2.1*i*0.5)
            + 7*math.sin(x/23 + 1.3 + i))
bands = [f'<rect x="-10" y="-10" width="{W+20}" height="{H+20}" fill="{cols[0]}"/>']
lines = []
for i in range(1, nb):
    pts = [(x, curve(i, x)) for x in range(-10, W+11, 6)]
    d = "M" + " L".join(f"{x},{y:.1f}" for x,y in pts)
    bands.append(f'<path d="{d} L{W+10},{H+10} L-10,{H+10} Z" fill="{cols[i]}"/>')
    lines.append(f'<path d="{d}" fill="none" stroke="#ffffff" stroke-opacity="0.35" stroke-width="1.6"/>')

# bat silhouette (right half, mirrored), centred at 0,0
R = [(0,-11),(3.5,-22),(7,-10),("Q",(10,-5),(11,-2)),
     ("Q",(30,-12),(48,-34)),("Q",(68,-32),(88,-26)),
     ("Q",(78,-14),(77,0)),("Q",(67,-6),(58,8)),("Q",(47,2),(38,14)),("Q",(27,6),(15,13)),
     ("Q",(11,20),(6,25)),("Q",(3,27),(0,28))]
def mirror(seg):
    if isinstance(seg[0], str): return ("Q", (-seg[1][0], seg[1][1]), (-seg[2][0], seg[2][1]))
    return (-seg[0], seg[1])
def batpath():
    d = f"M{R[0][0]},{R[0][1]}"
    for s in R[1:]:
        d += f" Q{s[1][0]},{s[1][1]} {s[2][0]},{s[2][1]}" if s[0]=="Q" else f" L{s[0]},{s[1]}"
    # walk back up the left side: reverse right side mirrored
    segs = R[:]  # points in order; build reverse
    pts = []
    for s in R:
        pts.append(s)
    # reverse traversal: need end points and controls swapped
    ends = []
    for s in R:
        ends.append(s[2] if s[0]=="Q" else s)
    for k in range(len(R)-1, 0, -1):
        s = R[k]; prev_end = ends[k-1]
        mp = (-prev_end[0], prev_end[1])
        if s[0]=="Q":
            c = (-s[1][0], s[1][1]); d += f" Q{c[0]},{c[1]} {mp[0]},{mp[1]}"
        else:
            d += f" L{mp[0]},{mp[1]}"
    return d + " Z"
bat = batpath()

# trajectory from bat to pin
bx, by, bs, rot = 316, 112, 1.2, -6
px, py = 190, 296
traj = f'M{bx-6},{by+40} C{bx-30},{by+110} {px+60},{py-150} {px+4},{py-86}'

pin = (f'<g transform="translate({px},{py})">'
       f'<ellipse cx="0" cy="2" rx="30" ry="9" fill="none" stroke="#ffffff" stroke-width="3" stroke-opacity="0.9"/>'
       f'<ellipse cx="0" cy="2" rx="52" ry="16" fill="none" stroke="#ffffff" stroke-width="2" stroke-opacity="0.5"/>'
       f'<path d="M0,0 C-6,-16 -24,-30 -24,-48 A24,24 0 1 1 24,-48 C24,-30 6,-16 0,0 Z" fill="#c8102e" stroke="#ffffff" stroke-width="4" stroke-linejoin="round"/>'
       f'<circle cx="0" cy="-48" r="9" fill="#ffffff"/></g>')

svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
<defs><clipPath id="hex"><path d="{hexpath(hexpts)}"/></clipPath>
<filter id="sh" x="-20%" y="-20%" width="140%" height="140%"><feDropShadow dx="0" dy="3" stdDeviation="3" flood-color="#000" flood-opacity="0.35"/></filter></defs>
<g clip-path="url(#hex)">
{chr(10).join(bands)}
{chr(10).join(lines)}
<path d="{traj}" fill="none" stroke="#ffffff" stroke-width="4" stroke-dasharray="2 10" stroke-linecap="round"/>
<g filter="url(#sh)">{pin}</g>
<g transform="translate({bx},{by}) rotate({rot}) scale({bs})" filter="url(#sh)"><path d="{bat}" fill="#10212b" stroke="#ffffff" stroke-width="2.4" stroke-linejoin="round"/></g>
<text x="259" y="478" text-anchor="middle" font-family="Inter Display" font-weight="900" font-size="112" letter-spacing="-2" fill="#ffffff" filter="url(#sh)">IsoriX</text>
</g>
<path d="{hexpath(hexpts)}" fill="none" stroke="#0a2f45" stroke-width="{2*inset+2}" clip-path="url(#hex)"/>
</svg>'''
open("logo.svg","w").write(svg)
