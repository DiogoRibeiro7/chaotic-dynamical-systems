"""Utilities for recurrence plot analysis.

This module complements the R package with a minimal Python
implementation. It uses Google-style docstrings so that tools
like Sphinx can generate documentation automatically.
"""

from __future__ import annotations

import numpy as np

__all__ = ["recurrence_plot", "recurrence_analysis"]


def recurrence_plot(x, embed: int = 2, delay: int = 1, eps: float | None = None) -> np.ndarray:
    """Compute a simple recurrence plot.

    Args:
        x (array_like): Time series samples.
        embed (int, optional): Embedding dimension. Defaults to 2.
        delay (int, optional): Delay between coordinates. Defaults to 1.
        eps (float, optional): Recurrence threshold. Defaults to 10% of the standard deviation of ``x``.

    Returns:
        numpy.ndarray: Boolean recurrence matrix.
    """
    x = np.asarray(x)
    n = len(x) - (embed - 1) * delay
    if n <= 0:
        raise ValueError("time series too short for chosen embedding")
    emb = np.column_stack([x[i : i + n] for i in range(0, embed * delay, delay)])
    dists = np.linalg.norm(emb[:, None, :] - emb[None, :, :], axis=2)
    if eps is None:
        eps = 0.1 * np.std(x)
    return dists <= eps


def recurrence_analysis(
    x, embed: int = 2, delay: int = 1, eps: float | None = None, lmin: int = 2
) -> dict:
    """Compute recurrence rate and determinism.

    Args:
        x (array_like): Time series samples.
        embed (int, optional): Embedding dimension. Defaults to 2.
        delay (int, optional): Delay between coordinates. Defaults to 1.
        eps (float, optional): Recurrence threshold. Defaults to 10% of the standard deviation of ``x``.
        lmin (int, optional): Minimum diagonal length. Defaults to 2.

    Returns:
        dict: ``{"recurrence_rate", "determinism", "recurrence_matrix"}``.
    """
    rp = recurrence_plot(x, embed=embed, delay=delay, eps=eps)
    n = rp.shape[0]
    rr = rp.sum() / float(n * n)

    diag_lens = []
    for k in range(-(n - 1), n):
        diag = np.diag(rp, k=k)
        if diag.size == 0:
            continue
        mask = np.concatenate(([diag[0]], diag[1:] != diag[:-1], [True]))
        idx = np.where(mask)[0]
        lens = np.diff(idx)[diag[idx[:-1]]]
        diag_lens.extend(lens)
    diag_lens = [l for l in diag_lens if l >= lmin]
    det = sum(diag_lens) / rp.sum() if diag_lens else 0.0

    return {
        "recurrence_rate": rr,
        "determinism": det,
        "recurrence_matrix": rp,
    }
