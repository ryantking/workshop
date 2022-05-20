package funcy

// Contains checks a list contains an item.
func Contains[T comparable](xs []T, v T) bool {
	for _, x := range xs {
		if x == v {
			return true
		}
	}

	return false
}

// Map does a functional map operation
func Map[T1, T2 any](xs []T1, fn func(x T1) T2) []T2 {
	ys, _ := EMap(xs, func(x T1, _ error) (T2, error) {
		return fn(x), nil
	})
	return ys
}

// EMap does a functional map, but passes any error into the next map call.
// The final error is returned.
func EMap[T1, T2 any](xs []T1, fn func(x T1, err error) (T2, error)) ([]T2, error) {
	var (
		ys  = make([]T2, len(xs))
		err error
	)
	for i, x := range xs {
		ys[i], err = fn(x, err)
	}
	if err != nil {
		return nil, err
	}

	return ys, nil
}

// Filter removes any values for which f evaluates true.
func Filter[T any](xs []T, f func(x T) bool) []T {
	filtered := make([]T, 0, len(xs))
	for i := range xs {
		if !f(xs[i]) {
			filtered = append(filtered, xs[i])
		}
	}

	return filtered
}

// Reduce converts a slice of T to M.
func Reduce[T, M any](s []T, f func(M, T) M, init M) M {
	acc := init
	for _, v := range s {
		acc = f(acc, v)
	}

	return acc
}
