import typing as tp

import numpy as np
from scipy.stats import norm, rankdata


def permutation_method(first_data: np.ndarray, second_data: np.ndarray,
                       statistic: tp.Callable[[np.ndarray, np.ndarray], float],
                       permutations_number: int) -> float:
    true_statistic_value = statistic(first_data, second_data)
    united_data = np.concatenate([first_data, second_data])
    p_value = 0
    for _ in range(permutations_number):
        data_permutation = np.random.permutation(united_data)
        stat_value = statistic(data_permutation[:first_data.size],
                               data_permutation[first_data.size:])
        p_value += stat_value > true_statistic_value
    return p_value / permutations_number


def BWStest_statistic(first_data: np.ndarray,
                      second_data: np.ndarray) -> float:
    if not first_data.size or not second_data.size:
        raise Exception("Data is empty!")

    united_data = np.concatenate([first_data, second_data])
    united_ranks = rankdata(united_data, method='ordinal')
    first_data_ranks = united_ranks[:first_data.size]
    second_data_ranks = united_ranks[first_data.size:]

    first_data_ranks.sort()
    second_data_ranks.sort()
    n = first_data.size
    m = second_data.size

    B_X = sum((rank - (i+1)*(n+m)/n)**2 / ((i+1) * (1 - (i+1)/(n+1)))
              for i, rank in enumerate(first_data_ranks))
    B_X *= (n+1) / (m * (n+m))

    B_Y = sum((rank - (i+1)*(n+m)/m)**2 / ((i+1) * (1 - (i+1)/(m+1)))
              for i, rank in enumerate(second_data_ranks))
    B_Y *= (m+1) / (n * (n+m))

    return (B_X+B_Y) / 2


def main() -> None:
    first_data = norm.rvs(loc=1, scale=1, size=100)
    second_data = norm.rvs(loc=1, scale=1, size=100)

    p_value = permutation_method(first_data, second_data,
                                 BWStest_statistic, 100)
    print(p_value)


if __name__ == "__main__":
    main()
