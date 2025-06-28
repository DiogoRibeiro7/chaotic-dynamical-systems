"""Utilities for simulating the Hénon map."""

from typing import List, Tuple


def simulate_henon_map(n: int, a: float = 1.4, b: float = 0.3,
                       x0: float = 0.0, y0: float = 0.0) -> Tuple[List[float], List[float]]:
    """Generate an orbit of the Hénon map.

    Args:
        n: Number of iterations to generate. Must be positive.
        a: Nonlinearity parameter ``a``.
        b: Contraction parameter ``b``.
        x0: Initial ``x`` value.
        y0: Initial ``y`` value.

    Returns:
        Tuple of two lists containing ``x`` and ``y`` coordinates.
    """
    assert n > 0, "n must be positive"
    x = [0.0] * n
    y = [0.0] * n
    x[0] = x0
    y[0] = y0
    for i in range(n - 1):
        x[i + 1] = 1 - a * x[i] ** 2 + y[i]
        y[i + 1] = b * x[i]
    return x, y


if __name__ == "__main__":
    xs, ys = simulate_henon_map(10)
    print(list(zip(xs[:3], ys[:3])))

