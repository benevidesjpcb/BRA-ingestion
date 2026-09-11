# Panel logos

Drop the two logo files here and the panel header picks them up on any machine:

    assets/logos/decea.png          (or .svg / .jpg)
    assets/logos/eurocontrol.svg

Matched case-insensitively by name, so `DECEA.PNG` works too. With a file
missing the header renders a marked slot saying which logo it is, rather than a
hand-drawn approximation of an official emblem — an emblem redrawn from memory
misrepresents the organisation it belongs to.

They are embedded into the HTML as data URIs, so the page stays a single file
with no external requests. Keep them small; an SVG or a PNG a few hundred
pixels wide is plenty at the 34-pixel height the header uses.

To use a file from somewhere else instead:

    options(totalbr.logo.decea = "/some/other/path/decea.png")
