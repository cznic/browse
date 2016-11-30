// errorcheck

// Copyright 2009 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Based on proposal by Ian Lance Taylor available at
//
//  https://github.com/golang/proposal/blob/master/design/15292-generics.md

// For now only a syntax check.

package p

import (
	"sort"
)

// Dummy types.
type ( //TODO-
	t int
	u int
	a int
	b int
	keytype int
	valtype int
)

type _ sort.Interface«int»

/*
	https://github.com/golang/proposal/blob/master/design/15292/2010-06-type-functions.md
*/

type Vector«t» []t
type Vector2«t, u» []struct{t; u}
type VectorInt Vector«int»
type VectorInt2 Vector2«int, string»

func Bound(v Vector«int»)
func Unbound«t»(v Vector«t»)
var UnboundInt = Unbound«int»

var fn = func(Vector«int»)() {}
var fn2 = func(v Vector«int») {}
var fn3 = func«t»(Vector«t») {} // ERROR "anonymous functions cannot have generic type parameters"

func (v Vector«int») BoundMethod(x int)
func (v Vector«t») UnboundMethod«t»(x t)

// func Generic(v t type) t
func Generic«t»(v t) t

// func Head(v Vector(t type)) {
//         var first t
//         first = v[0]
// }
func Head«t»(v Vector«t») {
	var first t
	first = v[0]
}

// func SetHead(v Vector(t type), e t) t {
//         v[0] = e
//         return e
// }
func SetHead«t»(v Vector«t», e t) t {
	v[0] = e
	return e
}

// generic(t) func SetHead(v Vector(t), e t) t { ... }
func SetHead2«t»(v Vector«t», e t) t {}

// func Choose(which bool, a t type, b t) t
func Choose«t»(which bool, a, b t) t

// func Repeat(x t type, n int) Slice(t) {
//         a := make(Slice(t), n)
//         for i := range a {
//                 a[i] = x
//         }
//         return a
// }
type Slice«t» []t
func Repeat«t»(x t, n int) Slice«t» {
	a := make(Slice«t», n)
	for i := range a {
		a[i] = x
	}
	return a
}

// type Pair(a, b) struct {
//         first a
//         second b
// }
// func Sum(a Pair(Vector(t type), Vector(t))) Vector(t)
type Pair«a, b» struct {
	first a
	second b
}
func Sum«t»(a Pair«Vector«t», Vector«t»») Vector«t»

// func (v Vector(t type)) At(int i) t {
//         return v[i]
// }
// 
// func (v Vector(t type)) Set(i int, x t) {
//         v[i] = x
// }
func (v Vector«t») At«t»(i int) t {
	return v[i]
}

func (v Vector«t») Set«t»(i int, x t) {
	v[i] = x
}

// package hashmap
// 
// type bucket(keytype, valtype) struct {
//         next *bucket(keytype, valtype)
//         key keytype
//         val valtype
// }
// 
// type Hashfn(keytype) func(keytype) uint
// 
// type Eqfn(keytype) func(keytype, keytype) bool
// 
// type Hashmap(keytype, valtype) struct {
//         hashfn Hashfn(keytype)
//         eqfn Eqtype(keytype)
//         buckets []bucket(keytype, valtype)
//         entries int
// }
// 
// func New(hashfn Hashfn(keytype type), eqfn Eqfn(keytype),
//          _ valtype type) *Hashmap(keytype, valtype) {
//         return &Hashmap(k, v){hashfn, eqfn,
//                 make([]bucket(keytype, valtype), 16),
//                 0}
// }
// 
// // Note that the dummy valtype parameter in the New function
// // exists only to get valtype into the function signature.
// // This feels wrong.
// 
// func (p *Hashmap(keytype type, vvaltype type))
//           Lookup(key keytype) (found bool, val valtype) {
//         h := p.hashfn(key) % len(p.buckets)
//         for b := buckets[h]; b != nil; b = b.next {
//                 if p.eqfn(key, b.key) {
//                         return true, b.val
//                 }
//         }
//         return
// }
type bucket«keytype, valtype» struct {
	next *bucket«keytype, valtype»
	key keytype
	val valtype
}

type Hashfn«keytype» func(keytype) uint

type Eqfn«keytype» func(keytype, keytype) bool

type Hashmap«keytype, valtype» struct {
	hashfn Hashfn«keytype»
	eqfn Eqfn«keytype»
	buckets []bucket«keytype, valtype»
	entries int
}

func New«keytype, valtype»(hashfn Hashfn«keytype», eqfn Eqfn«keytype») *Hashmap«keytype, valtype» {
	return &Hashmap«keytype, valtype»{
		hashfn,
		eqfn,
		make([]bucket«keytype, valtype», 16),
		0,
	}
}

func (p *Hashmap«keytype, vvaltype») Lookup(key keytype) (found bool, val valtype) {
	h := p.hashfn(key) % len(p.buckets)
	for b := p.buckets[h]; b != nil; b = b.next {
		if p.eqfn(key, b.key) {
			return true, b.val
		}
	}
	return
}

// Concepts: Try to get away w/o them. (???)

// type PrintableVector(t Stringer) []t
// func Concat(p PrintableVector(t type)) string {
//         s := ""
//         for _, v := range p {
//                 s += v.String()
//         }
//         return s
// }
func Concat(p Vector«t») string {
	s := ""
	for _, v := range p {
		s += v.String()
	}
	return s
}

// type Lesser(t) interface {
//         Less(t) bool
// }
// func Min(a, b t type Lesser(t)) t {
//         if a.Less(b) {
//                 return a
//         }
//         return b
// }
type Lesser«t» interface {
	Less(t) bool
}
func Min«t»(a, b t) t {
	// Type switch optimized away at compile time if t is known to be/not to be a Lesser«t».
	switch x := a.(type) {
	case Lesser«t»:
		if x.Less(b) {
			return a
		}

		return b
	default:
		if a < b {
			return a
		}

		return b
	}
}

// type Addable(t) interface {
//         Binary+(t) t
// }
// type AddableSlice(t Addable(t)) []t
// func Sum(v AddableSlice) t {
//         var sum t
//         for _, v := range v {
//                 sum = sum + v
//         }
//         return sum
// }
type Addable«t» interface {
	Add(t) t
}
func Sum244«t»(v Vector«t») t {
	// Type switch optimized away at compile time if t is known to be/not to be a Addable«t».
	switch t.(type) {
	case Addable«t»:
		var sum t
		for _, v := range v {
		        sum = sum.Add(v)
		}
		return sum
	default:
		var sum t
		for _, v := range v {
		        sum = sum + v
		}
		return sum
	}
}

/*
	https://github.com/golang/proposal/blob/master/design/15292/2011-03-gen.md
*/

//gen [T] type Vector []T
type Vector265«t» []t

// type VectorInt Vector[int]
// var v1 Vector[int]
// var v2 Vector[float32]
// gen [T1, T2] type Pair struct { first T1; second T2 }
// var v3 Pair[int, string]
type VectorInt272 Vector«int»
var v273 Vector«int»
var v274 Vector«float32»
type Pair275«t, u» struct { first t; second u }
var v276 Pair«int, string»

// gen [T] func SetHead(v Vector[T], e T) T {
//     v[0] = e
//     return e
// }
func SetHead282«t»(v Vector«t», e t) t {
	v[0] = e
	return e
}

// gen [T1, T2] (
// 	type Pair struct { first T1; second T2 }
// 
// 	func MakePair(first T1, second T2) Pair {
// 	    return &Pair{first, second}
// 	}
// )
type Pair297«t, u» struct { first t; second u }

func MakePair299«t, u»(first t, second t) Pair297«t, u» {
	return &Pair297«t, u»{first, second}
}

// var MakeIntPair = MakePair[int, int]
// var IntPairZero = MakeIntPair(0, 0)
var MakeIntPair305 = MakePair299«int, int»
var IntPairZero306 = MakePair299«int, int»(0, 0)
var IntPairZero307 = MakeIntPair305(0, 0)

// gen [T] func (v *Vector[T]) SetHeadMethod(e T) T {
//     v[0] = e
//     return e
// }
func (v *Vector«t») SetHeadMethod313«t»(e t) t {
	(*v)[0] = e
	return e
}

// gen [T, T2] func (v *Vector[T]) Transform(f func(T) T2) Vector[T2]
func (v *Vector«t, u») Transform«t, u»(f func(t) u) Vector«u»

// var v1 = MakePair[int, int](0, 0)
// var v2 = MakePair(0, 0)         // [int, int] deduced.
var v323 = MakePair299«int, int»(0, 0)
var v324 = MakePair299(0, 0)         // [int, int] deduced.

// gen [T] func SliceAverage(a []T) T {
//     s := T(0)
//     for _, v = range a {
//         s += v
//     }
//     return s / len(a)
// }
func SliceAverage«t»(a []t) t {
	s := t(0)
	// or: var s t
	for _, v := range a {
		s += v
	}
	return s / len(a)
}

// gen [T] type Number interface {
//     Plus(T) T
//     Divide(T) T
// }
// 
// gen [T Number[T]] func SliceAverage(a []T) T {
//     s := 0.[T]
//     for _, v = range a {
//         s = s.Plus(v)
//     }
//     return s.Divide(len(a))
// }
type Number354«t» interface {
	Plus(t) t
	Divide(t) t
}

func SliceAverage359«t»(a []t) t {
	// Type switch optimized away at compile time if t is known to be/not to be a Lesser«t».
	switch t.(type) {
	case Number354«t»:
		var s t
		for _, v = range a {
			s = s.Plus(v)
		}
		return s.Divide(t(len(a)))
	default:
		var s t
		for _, v := range a {
			s += v
		}
		return s / len(a)
	}
}

/*
	https://github.com/golang/proposal/blob/master/design/15292/2013-10-gen.md
*/

// var v1 = MakePair[int, int](0, 0)
// var v2 = MakePair(0, 0)         // [int, int] deduced.
// var v3 = Pair{0, 0}         // [int, int] deduced.
var v386 = MakePair299«int, int»(0, 0)
var v387 = MakePair299(0, 0)         // «int, int» deduced.
var v388 = Pair297{0, 0}         // «int, int» deduced.

// func SortSlice(s []T, less func(T, T) bool) {
//     sort.Sort(&sorter{s, less})
// }
// 
// type sorter struct {
//     s []T
//     less func(T, T) bool
// }
// 
// func (s *sorter) Len() int { return len(s.s) }
// func (s *sorter) Less(i, j int) bool { return s.less(s[i], s[j]) }
// func (s *sorter) Swap(i, j int) { s.s[i], s.s[j] = s.s[j], s.s[i] }
// 
// ) // End gen
func SortSlice«t»(s []t, less func(t, t) bool) {
    sort.Sort(&sorter{s, less})
}

type sorter«t» struct {
    s []t
    less func(t, t) bool
}

func (s *sorter«t») Len«t»() int           { return len(s.s) }
func (s *sorter«t») Less«t»(i, j int) bool { return s.less(s.s[i], s.s[j]) }
func (s *sorter«t») Swap«t»(i, j int)      { s.s[i], s.s[j] = s.s[j], s.s[i] }

// gen [T] func SortNumericSlice(s []T) {
//     SortSlice(s, func(a, b T) bool { return a < b })
// }
func SortNumericSlice«t»(s []t) {
    SortSlice(s, func(a, b t) bool { return a < b })
}

// gen [T] func Merge(a, b <-chan T) <-chan T {
//     c := make(chan T)
//     go func(a, b chan T) {
//         for a != nil && b != nil {
//             select {
//             case v, ok := <-a:
//                 if ok {
//                     c <- v
//                 } else {
//                     a = nil
//                 }
//             case v, ok := <-b:
//                 if ok {
//                     c <- v
//                 } else {
//                     b = nil
//                 }
//             }
//         }
//         close(c)
//     }(a, b)
//     return c
// }
func Merge«t»(a, b <-chan t) <-chan t {
	c := make(chan t)
	go func(a, b chan t) {
		for a != nil && b != nil {
			select {
			case v, ok := <-a:
				if ok {
					c <- v
				} else {
					a = nil
				}
			case v, ok := <-b:
				if ok {
					c <- v
				} else {
					b = nil
				}
			}
		}
		close(c)
	}(a, b)
	return c
}

// type [T] List struct { element T; next *List[T] }
type List«t» struct { element t; next *List«t» }

// type ListInt List[int]
// var v1 List[int]
// var v2 List[float]
// type (
// [T1, T2] MyMap map[T1]T2
// [T3] MyChan chan T3
// )
// var v3 MyMap[int, string]
type ListInt List«int»
var v483 List«int»
var v484 List«float»
type (
	MyMap«t, u» map[t]u
	MyChan«t» chan t
)
var v489 MyMap«int, string»

// func [T] Push(l *List[T], e T) *List[T] {
//     return &List[T]{e, l}
// }
func Push«t»(l *List«t», e t) *List«t» {
    return &List«t»{e, l}
}

// func [T] (v *List[T]) Push(e T) {
//     *v = &List[T]{e, v}
// }
func (v *List«t») Push«t»(e t) {
    *v = &List«t»{e, v}
}

// type [T] SortableSlice []T
// func [T] (v SortableSlice[T]) Len() int { return len(v) }
// func [T] (v SortableSlice[T]) Swap(i, j int) {
//     v[i], v[j] = v[j], v[i]
// }
// func [T] (v SortableSlice[T]) Less(i, j int) bool {
//     return v[i].Less(v[j])
// }
// func [T] (v SortableSlice[T]) Sort() {
//     sort.Sort(v)
// }
type SortableSlice«t» []t
func (v SortableSlice«t») Len«t»() int { return len(v) }
func (v SortableSlice«t») Swap«t»(i, j int) {
	v[i], v[j] = v[j], v[i]
}
func (v SortableSlice«t») Less«t»(i, j int) bool {
	// Type switch optimized away at compile time if possible.
	switch t.(type) {
	case Lesser«t»:
		return v[i].Less(v[j])
	default:
		return v[i] < v[j]
	}
}
func (v SortableSlice«t») Sort«t»() {
	sort.Sort(v)
}
