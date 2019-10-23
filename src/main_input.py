def MAGIC_EQ(x, y):
	return x == y

def Banana(c):
	b = c + 7
	d = 10
	a = c
	if (MAGIC_EQ(c, b)):
		a = 2 * c
	else:
		a = 5 + b
	if (MAGIC_EQ(a, d)):
		raise("Oh no")
Banana(10)

