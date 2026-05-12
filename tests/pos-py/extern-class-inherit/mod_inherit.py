class Mod(object):
    def __init__(self, seed):
        self._seed = seed

    def get_seed(self):
        return self._seed

    def describe(self):
        return f"Mod(seed={self._seed})"


class Counter(object):
    def __init__(self, start):
        self._count = start

    def bump(self):
        self._count += 1
        return self._count
