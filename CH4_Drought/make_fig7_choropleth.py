#!/usr/bin/env python3
# make_fig7_choropleth.py
# -----------------------------------------------------------------------------
# Figure 7: continental contribution to projected extreme-driven wetland CH4 by
# 2100, on REPAIRED Natural Earth continent boundaries.
#
# The ne_50m_continents shapefile has broken geometry that draws horizontal
# streaks across a Robinson map: (1) invalid self-intersecting rings, (2) thin
# degenerate slivers spanning the whole width, (3) dateline-spanning rings, and
# (4) an Arctic gap where the Europe polygon is cut at 65 N. This script repairs
# all four, then renders the choropleth. Verified visually to be streak-free
# while preserving mid-latitude coastlines (only >55 N is lightly smoothed).
#
# Usage:  python3 make_fig7_choropleth.py [SSP]     (default SSP2-4.5)
# Inputs: data/shapefiles/natural_earth/ne_50m_continents/ne_50m_continents.shp
#         outputs/regional_projection/continent_contributions_2100.csv  (from 14)
# Output: outputs/regional_projection/figures/continent_choropleth.png
# Requires: geopandas, shapely, matplotlib  (pip install geopandas matplotlib)
# -----------------------------------------------------------------------------
import os, sys
import geopandas as gpd, shapely, pandas as pd
from shapely.ops import unary_union
from shapely.geometry import box, LineString
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm, LinearSegmentedColormap

HERE = os.path.dirname(os.path.abspath(__file__))
SHP  = os.path.join(HERE, "data", "shapefiles", "natural_earth",
                    "ne_50m_continents", "ne_50m_continents.shp")
CSV  = os.path.join(HERE, "outputs", "regional_projection", "continent_contributions_2100.csv")
OUT  = os.path.join(HERE, "outputs", "regional_projection", "figures", "continent_choropleth.png")
SSP  = sys.argv[1] if len(sys.argv) > 1 else "SSP2-4.5"

def repair(geom):
    """Repair one continent MultiPolygon: drop sliver/dateline artifacts,
    dissolve, and close Arctic (>55N) digitization gaps only."""
    parts = list(geom.geoms) if geom.geom_type == "MultiPolygon" else [geom]
    keep = []
    for p in parts:
        w = p.bounds[2] - p.bounds[0]; ys = p.bounds[3] - p.bounds[1]
        if w > 0 and p.area / w < 0.3:        continue   # degenerate ribbon sliver
        if w > 180 and p.bounds[0] < -160:    continue   # spurious dateline-spanning ring
        if ys < 1.0 and w > 5:                continue   # thin horizontal sliver
        keep.append(p)
    u = unary_union(keep) if keep else geom
    north = u.intersection(box(-180, 55, 180, 90)).buffer(2.0, join_style=1).buffer(-2.0, join_style=1)
    south = u.intersection(box(-180, -90, 180, 55))
    return shapely.make_valid(unary_union([south, north]))

g = gpd.read_file(SHP)
g = g[g.continent != "Antarctica"].copy()
g["geometry"] = g.geometry.make_valid()
g["geometry"] = g.geometry.apply(repair)

bd = pd.read_csv(CSV)
bd = bd[bd.ssp == SSP].set_index("continent")["additional_Tg_per_yr_2100"]
g["val"] = g.continent.map(bd.to_dict())
lim = float(bd.abs().max())

rob = g.to_crs("ESRI:54030")
cmap = LinearSegmentedColormap.from_list("bwb", ["#8c510a", "#dfc27d", "#f7f7f7", "#92c5de", "#2166ac"])
norm = TwoSlopeNorm(vmin=-lim, vcenter=0, vmax=lim)

fig, ax = plt.subplots(figsize=(9, 4.7), dpi=300)
grat  = [LineString([(lon, y) for y in range(-88, 89, 2)]) for lon in range(-150, 181, 30)]
grat += [LineString([(x, lat) for x in range(-180, 181, 2)]) for lat in range(-60, 91, 30)]
gpd.GeoDataFrame(geometry=grat, crs=4326).to_crs("ESRI:54030").plot(
    ax=ax, color="grey", linewidth=0.2, alpha=0.45, zorder=1)
rob.plot(ax=ax, column="val", cmap=cmap, norm=norm, edgecolor="#4d4d4d", linewidth=0.2, zorder=2)
ax.set_axis_off(); ax.margins(0.01)
ax.text(0.0, 1.05, f"Continental contribution to global extreme-driven CH$_4$ by 2100 ({SSP})",
        transform=ax.transAxes, fontsize=12, va="bottom")
ax.text(0.0, 1.005, "Region-weighted; continents sum to the global total (WAD2M allocation)",
        transform=ax.transAxes, fontsize=9.5, va="bottom", color="#444444")
sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm); sm.set_array([])
cb = fig.colorbar(sm, ax=ax, shrink=0.6, pad=0.015, aspect=17)
cb.set_label("Additional CH$_4$\n(Tg yr$^{-1}$, 2100)", fontsize=10); cb.ax.tick_params(labelsize=9)

os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=300, bbox_inches="tight", facecolor="white")
print("wrote", OUT, "for", SSP)
