"""
This module is designed to create heatmaps displaying confidence interval overlap (or lack
thereof).
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import dec_util


def heatmap(df, color_palette, cmap, title, path_out=None, show=False):
    """
    Creates a heatmap using the given DataFrame, color palette, color map,
    and title.
    """
    # Create figure.
    fig, ax = plt.subplots(figsize=(10, 10))
    # Heatmap.
    sns.heatmap(
        df,
        linewidths=2,
        square=True,
        cmap=cmap, cbar=False,
        ax=ax
    )
    # Cross-hatching to display information in
    # multiple ways for accessibility.
    range_plot = np.arange(len(df.columns) + 1)
    hatch_logical = np.ma.masked_not_equal(df.values, 10)
    plt.pcolor(
        range_plot, range_plot,
        hatch_logical,
        hatch="//", linewidth=30.0, alpha=0.0
    )
    # Tick marks.
    ax.xaxis.tick_top()
    plt.tick_params(left=False, bottom=False, top=False)
    tick_labels = [x.replace("Men ", "") for x in dec_util.events]
    plt.xticks(ticks=[i + 0.5 for i in range(10)], labels=tick_labels, fontsize=16)
    plt.yticks(ticks=[i + 0.5 for i in range(10)], labels=tick_labels, fontsize=16)
    # Custom legend.
    # legend_lines = [Line2D([0], [0], color=color_palette[0]),
    #                 Line2D([0], [0], color=color_palette[2]),
    #                 Line2D([0], [0], color=color_palette[5])]
    # legend_elems = [Patch(facecolor=color_palette[0], label="Row Coefficient is Greater"),
    #                 Patch(facecolor=color_palette[2], label="No Difference"),
    #                 Patch(facecolor=color_palette[5], label="Column Coefficient is Greater")]
    # plt.legend(legend_lines, ["Row coefficient is Greater", "No Difference", "Column is coefficient Greater"])
    # plt.legend(handles=legend_elems, fontsize=14)
    # plt.title(title, fontsize=24)
    if path_out is not None:
        plt.savefig(path_out)
    if show:
        plt.show()

# Load data.
# df_log_first = pd.read_csv("CI_matrix_log_first.csv", index_col=0)
# df_inverse_first = pd.read_csv("CI_matrix_inverse_first.csv", index_col=0)
# df_identity_first = pd.read_csv("CI_matrix_identity_first.csv", index_col=0)
# df_log_delta = pd.read_csv("CI_matrix_log_delta.csv", index_col=0)
# df_inverse_delta = pd.read_csv("CI_matrix_inverse_delta.csv", index_col=0)
# df_identity_delta = pd.read_csv("CI_matrix_identity_delta.csv", index_col=0)

df_log_first = pd.read_csv("lh_matrix_log_first.csv", index_col=0)
df_inverse_first = pd.read_csv("lh_matrix_inv_first.csv", index_col=0)
df_identity_first = pd.read_csv("lh_matrix_ide_first.csv", index_col=0)
df_log_delta = pd.read_csv("lh_matrix_log_delta.csv", index_col=0)
df_inverse_delta = pd.read_csv("lh_matrix_inv_delta.csv", index_col=0)
df_identity_delta = pd.read_csv("lh_matrix_ide_delta.csv", index_col=0)

df_log_first_CI = pd.read_csv("CI_matrix_log_first.csv", index_col=0)
df_inverse_first_CI = pd.read_csv("CI_matrix_inverse_first.csv", index_col=0)
df_identity_first_CI = pd.read_csv("CI_matrix_identity_first.csv", index_col=0)
df_log_delta_CI = pd.read_csv("CI_matrix_log_delta.csv", index_col=0)
df_inverse_delta_CI = pd.read_csv("CI_matrix_inverse_delta.csv", index_col=0)
df_identity_delta_CI = pd.read_csv("CI_matrix_identity_delta.csv", index_col=0)

df_log_first_boot_CI = pd.read_csv("boot_CI_matrix_log_fsb.csv", index_col=0)
df_inv_first_boot_CI = pd.read_csv("boot_CI_matrix_inv_fsb.csv", index_col=0)
df_ide_first_boot_CI = pd.read_csv("boot_CI_matrix_ide_fsb.csv", index_col=0)
df_log_delta_boot_CI = pd.read_csv("boot_CI_matrix_log_delta.csv", index_col=0)
df_inv_delta_boot_CI = pd.read_csv("boot_CI_matrix_inv_delta.csv", index_col=0)
df_ide_delta_boot_CI = pd.read_csv("boot_CI_matrix_ide_delta.csv", index_col=0)

# Create color palette.
h_neg = 220
# h_neg = 0
h_pos = 20
color_vals = (0.85, 0.4, 0.1)
color_palette = sns.diverging_palette(h_neg, h_pos)
# color_palette = sns.light_palette(color_vals, n_colors=5)
cmap = sns.diverging_palette(h_neg, h_pos, as_cmap=True)
# cmap = sns.light_palette(color_vals, n_colors=5, as_cmap=True)

# Define image output path.
path_img_out = "Images/gamma_analysis_r/first_delta_minus_outliers/"

# FSB heatmaps.
# heatmap(df_log_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Log Link", path_out=path_img_out + "log/coef_matrix_first.png")
# heatmap(df_inverse_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Inverse Link", path_out=path_img_out + "inverse/coef_matrix_first.png")
# heatmap(df_identity_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Identity Link", path_out=path_img_out + "identity/coef_matrix_first.png", show=True)

# captions = {
#     "fsb_log": "",
#     "fsb_inverse": "",
#     "fsb_identity": "",
#     "delta_log": "",
#     "delta_inverse": "",
#     "delta_identity": ""
# }

heatmap(df_log_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Log Link", path_out=path_img_out + "log/lh_matrix_first.png")
heatmap(df_inverse_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Inverse Link", path_out=path_img_out + "inverse/lh_matrix_first.png")
heatmap(df_identity_first, color_palette=color_palette, cmap=cmap, title="FSB Features, Identity Link", path_out=path_img_out + "identity/lh_matrix_first.png", show=True)

# Delta heatmaps.
heatmap(df_log_delta, color_palette=color_palette, cmap=cmap, title="Delta Features, Log Link", path_out=path_img_out + "log/lh_matrix_delta.png")
heatmap(df_inverse_delta, color_palette=color_palette, cmap=cmap, title="Delta Features, Inverse Link", path_out=path_img_out + "inverse/lh_matrix_delta.png")
heatmap(df_identity_delta, color_palette=color_palette, cmap=cmap, title="Delta Features, Identity Link", path_out=path_img_out + "identity/lh_matrix_delta.png")

# FSB heatmaps, CIs.
heatmap(df_log_first_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Log Link", path_out=path_img_out + "log/CI_matrix_first.png")
heatmap(df_inverse_first_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Inverse Link", path_out=path_img_out + "inverse/CI_matrix_first.png")
heatmap(df_identity_first_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Identity Link", path_out=path_img_out + "identity/CI_matrix_first.png", show=True)

# Delta heatmaps, CIs.
heatmap(df_log_delta_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Log Link", path_out=path_img_out + "log/CI_matrix_delta.png")
heatmap(df_inverse_delta_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Inverse Link", path_out=path_img_out + "inverse/CI_matrix_delta.png")
heatmap(df_identity_delta_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Identity Link", path_out=path_img_out + "identity/CI_matrix_delta.png")

# Heatmaps, boot CIs.
heatmap(df_log_first_boot_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Log Link", path_out=path_img_out + "log/boot_CI_matrix_first.png")
heatmap(df_inv_first_boot_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Inverse Link", path_out=path_img_out + "inverse/boot_CI_matrix_first.png")
heatmap(df_ide_first_boot_CI, color_palette=color_palette, cmap=cmap, title="FSB Features, Identity Link", path_out=path_img_out + "identity/boot_CI_matrix_first.png")

heatmap(df_log_delta_boot_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Log Link", path_out=path_img_out + "log/boot_CI_matrix_delta.png")
heatmap(df_inv_delta_boot_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Inverse Link", path_out=path_img_out + "inverse/boot_CI_matrix_delta.png")
heatmap(df_ide_delta_boot_CI, color_palette=color_palette, cmap=cmap, title="Delta Features, Identity Link", path_out=path_img_out + "identity/boot_CI_matrix_delta.png")

