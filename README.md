# purescript-diffy

(Almost) generic diffing of PureScript values.

## The Problem

Suppose your project has a test suite, and at some point you have code that looks like this:

```purescript
describe "doc test" do
	it "works" do
		x <- f
		y <- g

		x `shouldEqual` y
```

Maybe at some point you broke this test, and something really ugly gets spit out to your terminal.
Perhaps something like:

```
doc test
  ✗ fails:

[{"_id":"5c7db03a4b79eb8ff543b8e8","index":0,"guid":"9b5524cf-f631-4a23-9d41-658
41ca38a05","isActive":true,"balance":"$1,429.66","picture":"http://placehold.it/
32x32","age":20,"eyeColor":"blue","name":{"first":"Edith","last":"Dudley"},"comp
any":"KAGGLE","email":"edith.dudley@kaggle.net","phone":"+1 (850) 432-2305","add
ress":"267 Randolph Street, Fairview, Utah, 7360","about":"Esse dolor ipsum cons
ectetur eiusmod do. Eiusmod voluptate dolore enim enim elit minim dolor esse sun
t eu sint Lorem tempor irure. Non minim ut in cillum officia sint ea culpa adipi
sicing aliquip excepteur exercitation sint. Non laborum ea est labore laboris ad
 id eiusmod veniam non eiusmod est eu. Pariatur consequat ad nulla laborum. In l
aboris tempor minim magna consectetur commodo non in exercitation ad. Cillum ess
e pariatur aliqua esse deserunt proident quis esse exercitation consectetur do."
,"registered":"Wednesday, August 2, 2017 7:29 PM","latitude":"-49.192406","longi
tude":"29.179577","tags":["adipisicing","commodo","eu","qui","laborum"],"range":
[0,1,2,3,4,5,6,7,8,9],"friends":[{"id":0,"name":"Bennett Young"},{"id":1,"name":
"Barrett Fitzgerald"},{"id":2,"name":"Boyd Watson"}],"greeting":"Hello, Edith! Y
ou have 9 unread messages.","favoriteFruit":"banana"}] ≠ [{"_id":"5c7db03a8dc088
174af2dc2f","index":1,"guid":"2fb28e28-966d-4dc9-9e07-052dbdcf564c","isActive":f
alse,"balance":"$1,124.19","picture":"http://placehold.it/32x32","age":22,"eyeCo
lor":"blue","name":{"first":"Ramona","last":"Wise"},"company":"CEMENTION","email
":"ramona.wise@cemention.tv","phone":"+1 (861) 481-2436","address":"786 Chester
Court, Salvo, New Jersey, 7633","about":"Ut qui id ut dolor est adipisicing nisi
voluptate laboris. Sunt non proident cillum deserunt id cillum cupidatat laborum
labore velit. In aliqua minim enim ullamco nulla quis incididunt eiusmod.","regi
stered":"Thursday, August 7, 2014 2:47 AM","latitude":"-80.306633","longitude":"
170.225432","tags":["nisi","consectetur","velit","eu","incididunt"],"range":[0,1
,2,3,4,5,6,7,8,9],"friends":[{"id":0,"name":"Sherman Mckenzie"},{"id":1,"name":"
Foley Hester"},{"id":2,"name":"Isabella Walker"}],"greeting":"Hello, Ramona! You
have 7 unread messages.","favoriteFruit":"strawberry"}]
```

The test fails, and it tells you that `x` was not, in fact, equal to `y`.

But why?

Maybe after a bit of effort you'll find out that `x` has `"index": 0` and `y` has `"index": 1`.

Or maybe there is no difference in the output, since the relevant `Show` instance doesn't actually show all the relevant fields.

Then you might find yourself writing code like this:

```purescript
x :: NestedDataType <- f
y :: NestedDataType <- g

when (x /= y) $
	case x, y of
		[x'], [y'] -> do
			if x'._id /= y'._id
				then do ...
				else do ...
		_, _ -> pure unit
```

This can quickly become unwieldy and tough to keep your tests automatically updated with your data types, especially if they change often.

## The Solution

With this library, generics and row-based programming come to the rescue.

We can simply derive `Generic` and add another instance:

```purescript
derive instance genericNested :: Generic NestedDataType _

instance diffyNested :: Diffy NestedDataType where
	diffy f x y = genericDiffy f x y
```

Define a callback that's called when differences are found in the data type (Note: This callback will actually work with any data type that has a `Diffy` instance, it doesn't need to be tied to the `NestedDataType` definition).

```purescript
effectCb :: Diffy.Callback Effect
effectCb = mkCallback f g h
	where...
```

Then our test turns into the following:

```purescript
describe "doc test" do
	it "works" do
		x <- f
		y <- g

		diffy effectCb x y
```
