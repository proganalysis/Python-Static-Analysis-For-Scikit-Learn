def func2(x, y):
	y = y + 2
	x = x + 5
	func1(x, y)

def func1(c, t):
	z = z + 2
	b = t + 7
	k = 1
	d = k + 9
	a = c
	if (c == b):
		a = 2 * c
	else:
		a = c + 5
	if (a == d):
		raise("throw here")
func2(5, 7)
func1(10, 5)
