package tree

import (
	"fmt"
	"math/bits"

	i "github.com/cs-au-dk/goat/utils/indenter"

	"github.com/benbjohnson/immutable"
)

// NewTree constructs a fresh immutable key-value map with the specified hasher.
func NewTree[K, V any](hasher immutable.Hasher[K]) Tree[K, V] {
	return Tree[K, V]{hasher, nil}
}

// Tree is a Patricia tree implementation of an immutable hash map.
type Tree[K, V any] struct {
	hasher immutable.Hasher[K]
	root   node[K, V]
}

// hash computes the big-ending 32-bit hash key.
func (tree Tree[K, V]) hash(key K) keyt {
	// The paper claims that big-endian patricia trees work better than
	// little-endian trees in practice. Instead of modifying the functions
	// operating on the tree, we can get the benefits of a big-endian tree by
	// reversing the bit representation of hashes up-front.
	return bits.Reverse32(tree.hasher.Hash(key))
}

// Lookup the value stored at the given key.
func (tree Tree[K, V]) Lookup(key K) (V, bool) {
	// Hashing can be expensive, so we hash the key once here and pass it on.
	return lookup(tree.root, tree.hash(key), key, tree.hasher)
}

// Insert the given key-value pair into the map.
// Replaces previous value with the same key if it exists.
func (tree Tree[K, V]) Insert(key K, value V) Tree[K, V] {
	return tree.InsertOrMerge(key, value, nil)
}

// InsertOrMerge inserts the given key-value pair into the map, or combines it
// with any previous mapping `prevValue` for the key by inserting value
// `f(value, prevValue)` instead.
func (tree Tree[K, V]) InsertOrMerge(key K, value V, f mergeFunc[V]) Tree[K, V] {
	tree.root, _ = insert(tree.root, tree.hash(key), key, value, tree.hasher, f)
	return tree
}

// Remove a mapping for the given key if it exists.
func (tree Tree[K, V]) Remove(key K) Tree[K, V] {
	if _, ok := tree.Lookup(key); ok {
		tree.root = remove(tree.root, tree.hash(key), key, tree.hasher)
	}
	return tree
}

// Call the given function once for each key-value pair in the map.
func (tree Tree[K, V]) ForEach(f eachFunc[K, V]) {
	if tree.root != nil {
		tree.root.each(f)
	}
}

// Merge combines two maps. If both maps contain a value for a key, the resulting map
// will map the key to the result of `f` on the two values.
// `f` must be commutative and idempotent!
// This operation is made fast by skipping processing of shared subtrees.
// Merging a tree with itself after r updates should have complexity
// equivalent to `O(r * (keysize + f))`.
func (tree Tree[K, V]) Merge(other Tree[K, V], f mergeFunc[V]) Tree[K, V] {
	tree.root, _ = merge(tree.root, other.root, tree.hasher, f)
	return tree
}

// Equal checks whether two maps are equal. Values are compared with the provided
// function. This operation also skips processing of shared subtrees.
func (tree Tree[K, V]) Equal(other Tree[K, V], f cmpFunc[V]) bool {
	return equal(tree.root, other.root, tree.hasher, f)
}

// Size returns the number of key-value pairs in the map.
// NOTE: Runs in linear time in the size of the map.
func (tree Tree[K, V]) Size() (res int) {
	tree.ForEach(func(_ K, _ V) {
		res++
	})
	return
}

// StringFiltered returns a string representation of a map containing all elements
// that pass the given filter.
func (tree Tree[K, V]) StringFiltered(pred func(k K, v V) bool) string {
	buf := []func() string{}

	tree.ForEach(func(k K, v V) {
		if pred == nil || pred != nil && pred(k, v) {
			buf = append(buf, func() string {
				return fmt.Sprintf("%v ↦ %v", k, v)
			})
		}
	})

	return i.Indenter().Start("{").NestThunked(buf...).End("}")
}

func (tree Tree[K, V]) String() string {
	return tree.StringFiltered(nil)

}

// End of public interface

// The patricia tree implementation is based on:
// https://web.archive.org/web/20220515235749/http://ittc.ku.edu/~andygill/papers/IntMap98.pdf

type (
	// eachFunc is the type of procedures defined over elements of the tree.
	eachFunc[K, V any] func(key K, value V)
	// node is an interface defined over nodes in the Patricia tree.
	node[K, V any] interface {
		each(eachFunc[K, V])
	}

	// keyt is an alias over the key type of a Patricia tree.
	keyt = uint32

	// branch encodes a branching node in the Patricia tree.
	branch[K, V any] struct {
		prefix keyt // Common prefix of all keys in the left and right subtrees
		// A number with exactly one positive bit. The position of the bit
		// determines where the prefixes of the left and right subtrees diverge.
		branchBit keyt
		left      node[K, V]
		right     node[K, V]
	}
	// pair encodes a key-value pair in the Patricia tree.
	pair[K, V any] struct {
		key   K
		value V
	}
	// leaf encodes a terminal node in the Patricia tree.
	leaf[K, V any] struct {
		// The (shared) hash value of all keys in the leaf.
		key keyt
		// List of values to handle hash collisions.
		// TODO: Since collisions should be rare it might be worth
		// it to have a fast implementation when no collisions occur.
		values []pair[K, V]
	}
)

// each recursively applies a procedure over each element of the tree.
func (b *branch[K, V]) each(f eachFunc[K, V]) {
	b.left.each(f)
	b.right.each(f)
}

// match returns whether the key matches the prefix up until the branching bit.
// Intuitively: does the key belong in the branch's subtree?
func (b *branch[K, V]) match(key keyt) bool {
	return (key & (b.branchBit - 1)) == b.prefix
}

// copy constructs a new leaf that inherits the values of this leaf.
func (l *leaf[K, V]) copy() *leaf[K, V] {
	return &leaf[K, V]{
		l.key,
		append([]pair[K, V](nil), l.values...),
	}
}

// each applies a procedure to all values in the leaf.
func (l *leaf[K, V]) each(f eachFunc[K, V]) {
	for _, pr := range l.values {
		f(pr.key, pr.value)
	}
}

// br is a smart branch constructor that compresses the Patricia tree.
func br[K, V any](prefix, branchBit keyt, left, right node[K, V]) node[K, V] {
	if left == nil {
		return right
	} else if right == nil {
		return left
	}

	return &branch[K, V]{prefix, branchBit, left, right}
}

// lookup recursively searches a tree for the value found at a given key.
func lookup[K, V any](tree node[K, V], hash keyt, key K, hasher immutable.Hasher[K]) (ret V, found bool) {
	if tree == nil {
		return
	}

	switch tree := tree.(type) {
	case *leaf[K, V]:
		if tree.key == hash {
			for _, pr := range tree.values {
				if hasher.Equal(key, pr.key) {
					return pr.value, true
				}
			}
		}

		return

	case *branch[K, V]:
		rec := tree.right
		if !tree.match(hash) {
			return
		} else if zeroBit(hash, tree.branchBit) {
			rec = tree.left
		}

		return lookup(rec, hash, key, hasher)

	default:
		panic("Impossible: unknown tree root type.")
	}
}

// join merges two trees t0 and t1 which have prefixes p0 and p1 respectively.
// The prefixes must not be equal!
func join[K, V any](p0, p1 keyt, t0, t1 node[K, V]) node[K, V] {
	bbit := branchingBit(p0, p1)
	prefix := p0 & (bbit - 1)
	if zeroBit(p0, bbit) {
		return &branch[K, V]{prefix, bbit, t0, t1}
	} else {
		return &branch[K, V]{prefix, bbit, t1, t0}
	}
}

// mergeFunc represents functions that merge two values. Must be commutative and idempotent.
// The second return value informs the caller whether a == b.
//
// NOTE: This flag allows us to do some optimizations. Namely we can keep old
// nodes instead of replacing them with "equal" copies when the flag is true.
// However, it complicates the implementation a little bit - I'm not sure it's
// worth it.
type mergeFunc[V any] func(a, b V) (V, bool)

// insert a key-value pair into a tree. If `f` is nil the old value is always replaced with the argument value, otherwise
// the old value is replaced with `f(value, prevValue)`.
// If the returned flag is false, the returned node is (reference-)equal to the input node.
func insert[K, V any](tree node[K, V], hash keyt, key K, value V, hasher immutable.Hasher[K], f mergeFunc[V]) (node[K, V], bool) {
	if tree == nil {
		return &leaf[K, V]{key: hash, values: []pair[K, V]{{key, value}}}, true
	}

	var prefix keyt
	switch tree := tree.(type) {
	case *leaf[K, V]:
		if tree.key == hash {
			for i, pr := range tree.values {
				// If key matches previous key, replace value
				if hasher.Equal(key, pr.key) {
					newValue := value
					if f != nil {
						var equal bool
						newValue, equal = f(value, pr.value)

						if equal {
							return tree, false
						}
					}

					lf := tree.copy()
					lf.values[i].value = newValue
					return lf, true
				}
			}

			// Hash collision - append to list of values in leaf
			lf := tree.copy()
			lf.values = append(lf.values, pair[K, V]{key, value})
			return lf, true
		}

		prefix = tree.key

	case *branch[K, V]:
		if tree.match(hash) {
			l, r := tree.left, tree.right
			var changed bool
			if zeroBit(hash, tree.branchBit) {
				l, changed = insert(l, hash, key, value, hasher, f)
			} else {
				r, changed = insert(r, hash, key, value, hasher, f)
			}
			if !changed {
				return tree, false
			}
			return &branch[K, V]{tree.prefix, tree.branchBit, l, r}, true
		}

		prefix = tree.prefix

	default:
		panic(fmt.Sprintf("Unknown Patricia tree type: %T\nTree: %v", tree, tree))
	}

	newLeaf, _ := insert(nil, hash, key, value, nil, nil)
	return join(hash, prefix, newLeaf, tree), true
}

func remove[K, V any](tree node[K, V], hash keyt, key K, hasher immutable.Hasher[K]) node[K, V] {
	if tree == nil {
		return tree
	}

	switch tree := tree.(type) {
	case *leaf[K, V]:
		if tree.key == hash {
			newLeaf := &leaf[K, V]{tree.key, nil}
			// Copy all pairs that do not match the key
			for _, pr := range tree.values {
				if !hasher.Equal(key, pr.key) {
					newLeaf.values = append(newLeaf.values, pr)
				}
			}

			if len(newLeaf.values) == 0 {
				return nil
			}

			return newLeaf
		}
	case *branch[K, V]:
		if tree.match(hash) {
			left, right := tree.left, tree.right
			if zeroBit(hash, tree.branchBit) {
				left = remove(left, hash, key, hasher)
			} else {
				right = remove(right, hash, key, hasher)
			}
			return br(tree.prefix, tree.branchBit, left, right)
		}
	default:
		panic(fmt.Sprintf("Unknown Patricia tree type: %T\nTree: %v", tree, tree))
	}

	return tree
}

// merge two nodes. If the returned flag is true, a and b represent equal trees.
func merge[K, V any](a, b node[K, V], hasher immutable.Hasher[K], f mergeFunc[V]) (node[K, V], bool) {
	// Cheap pointer-equality
	if a == b {
		return a, true
	} else if a == nil {
		return b, false
	} else if b == nil {
		return a, false
	}

	// Check if either a or b is a leaf
	lf, isLeaf := a.(*leaf[K, V])
	other := b
	if !isLeaf {
		lf, isLeaf = b.(*leaf[K, V])
		other = a
	}

	if isLeaf {
		originalOther := other
		for _, pr := range lf.values {
			other, _ = insert(other, lf.key, pr.key, pr.value, hasher, f)
		}

		if oLf, oIsLeaf := other.(*leaf[K, V]); oIsLeaf &&
			other == originalOther &&
			len(lf.values) == len(oLf.values) {
			// Since the other tree is also a leaf, and it did not change as a
			// result of inserting our values, and we did not start out with a
			// fewer number of key-value pairs than the other leaf, the two
			// leaves were (and are still) equal.
			return a, true
		}

		return other, false
	}

	// Both a and b are branches
	s, t := a.(*branch[K, V]), b.(*branch[K, V])
	if s.branchBit == t.branchBit && s.prefix == t.prefix {
		l, leq := merge(s.left, t.left, hasher, f)
		r, req := merge(s.right, t.right, hasher, f)
		if leq && req {
			return s, true
		} else if (leq || l == s.left) && (req || r == s.right) {
			return s, false
		} else if (leq || l == t.left) && (req || r == t.right) {
			return t, false
		}

		return &branch[K, V]{s.prefix, s.branchBit, l, r}, false
	}

	if s.branchBit > t.branchBit {
		s, t = t, s
	}

	if s.branchBit < t.branchBit && s.match(t.prefix) {
		// s contains t
		l, r := s.left, s.right
		if zeroBit(t.prefix, s.branchBit) {
			l, _ = merge(l, node[K, V](t), hasher, f)
			if l == s.left {
				return s, false
			}
		} else {
			r, _ = merge(r, node[K, V](t), hasher, f)
			if r == s.right {
				return s, false
			}
		}
		return &branch[K, V]{s.prefix, s.branchBit, l, r}, false
	} else {
		// prefixes disagree
		return join(s.prefix, t.prefix, node[K, V](s), node[K, V](t)), false
	}
	// NOTE: The implementation of this function is complex because it is
	// performance critical, and since the performance does not rely only on
	// the implementation within this function. Using shared subtrees speeds
	// up future merge/equal operations on the result, which is important.
}

type cmpFunc[V any] func(a, b V) bool

func equal[K, V any](a, b node[K, V], hasher immutable.Hasher[K], f cmpFunc[V]) bool {
	if a == b {
		return true
	} else if a == nil || b == nil {
		return false
	}

	switch a := a.(type) {
	case *leaf[K, V]:
		b, ok := b.(*leaf[K, V])
		if !ok || len(a.values) != len(b.values) {
			return false
		}

	FOUND:
		for _, apr := range a.values {
			for _, bpr := range b.values {
				if hasher.Equal(apr.key, bpr.key) {
					if !f(apr.value, bpr.value) {
						return false
					}

					continue FOUND
				}
			}

			// a contained a key that b did not
			return false
		}

		return true

	case *branch[K, V]:
		b, ok := b.(*branch[K, V])
		if !ok {
			return false
		}

		return a.prefix == b.prefix && a.branchBit == b.branchBit &&
			equal(a.left, b.left, hasher, f) && equal(a.right, b.right, hasher, f)

	default:
		panic(fmt.Sprintf("Unknown node type: %T\nTree: %v", a, a))
	}
}
