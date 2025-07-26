import numpy as np
from chaoticds.recurrence import recurrence_plot, recurrence_analysis


def test_recurrence_plot_shape():
    rp = recurrence_plot(np.random.randn(10))
    # expect length minus embedding dimension plus one
    expected = 10 - 2 + 1
    assert rp.shape == (expected, expected)


def test_recurrence_analysis_keys():
    props = recurrence_analysis(np.random.randn(10))
    assert {'recurrence_rate', 'determinism', 'recurrence_matrix'} <= props.keys()
