"""
This module is designed to combine the different coefficient
plots produced by R so that they can be placed into a single
plot in the word document.
"""

import matplotlib.pyplot as plt


def make_combined_fig(path, type_="first", legend="", path_out=None):
    """
    Makes a combined coefficient plot.

    type_: "first" or "delta".
    """
    # Load the images for each linking function.
    imgs = [
        plt.imread(f"{path}{link}/coef_sorted_{type_}.png")
        for link in ["log", "inverse", "identity"]
    ]

    # Create combined plot.
    fig, axs = plt.subplots(ncols=3, nrows=1, figsize=(18, 10))

    # Show the images.
    [ax.imshow(img) for img, ax in zip(imgs, axs)]

    # Remove "spines".
    [ax.spines[['left', 'right', 'bottom', 'top']].set_visible(False)
     for ax in axs]

    # Remove ticks.
    [ax.tick_params(bottom=False, left=False, labelbottom=False, labelleft=False)
     for ax in axs]

    # Titles.
    axs[0].xaxis.set_label_position('bottom')
    axs[1].xaxis.set_label_position('bottom')
    axs[2].xaxis.set_label_position('bottom')
    axs[0].set_xlabel("(A) Log", fontsize=24)
    axs[1].set_xlabel("(B) Inverse", fontsize=24)
    axs[2].set_xlabel("(C) Identity", fontsize=24)

    fig.text(0, 0, s=legend, fontsize=24, wrap=True)

    plt.tight_layout(pad=2.0, w_pad=0.0)

    # Save.
    plt.savefig(path_out)


legend_first =\
"""
Figure 1: Absolute magnitudes of FSB coefficients, sorted from highest to lowest. Error bars indicate 95% confidence interval.
"""

legend_delta =\
"""
Figure 3: Absolute magnitudes of delta coefficients, sorted from highest to lowest. Error bars indicate 95% confidence interval.
"""

make_combined_fig(
    path="Images/gamma_analysis_r/first_delta_minus_outliers/",
    type_="first",
    legend=legend_first,
    path_out="Images/gamma_analysis_r/first_delta_minus_outliers/coef_sorted_first_combined.png"
)

make_combined_fig(
    path="Images/gamma_analysis_r/first_delta_minus_outliers/",
    type_="delta",
    legend=legend_delta,
    path_out="Images/gamma_analysis_r/first_delta_minus_outliers/coef_sorted_delta_combined.png"
)

