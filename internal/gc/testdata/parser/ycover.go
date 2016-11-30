// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cover

// Cover states not visited by yacc parser when parsing GOROOT/src.

import ()
import . "errors"
const()
type ()
type _ (_)
type _ [(<-chan (_))]_
type _ [(<-chan *_)]_
type _ [(<-chan <-chan _)]_
type _ [(<-chan _)]_
type _ [(<-chan chan _)]_
type _ [(<-chan func())]_
type _ [chan _('a',)]_
type _ chan(_)
var ()

// issue 15292.

type _«_» _
type _«_,» _«_»
type _«_» _«_,»
type _«_» _«_,_,»
