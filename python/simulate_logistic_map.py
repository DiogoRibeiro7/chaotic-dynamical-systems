"""Utilities for simulating the logistic map.

This module provides a simple function to generate orbits of the
logistic map.

"""

from typing import List


def simulate_logistic_map(n: int, r: float, x0: float) -> List[float]:
    """Generate a logistic map time series.

    Args:
        n: Number of iterations to produce. Must be positive.
        r: Growth rate parameter.
        x0: Initial value, typically in the open interval (0, 1).

    Returns:
        List of ``n`` floating point values representing the orbit.
    """
    assert n > 0, "n must be positive"
    x = [0.0] * n
    x[0] = x0
    for i in range(n - 1):
        x[i + 1] = r * x[i] * (1 - x[i])  # logistic iteration
    return x


if __name__ == "__main__":
    # Example usage when running this file directly
    series = simulate_logistic_map(10, 3.8, 0.2)
    print(series[:5])

