def MAGIC_EQ(x, y):
	return x == y

def Banana(c):
	b = c + 7
	x = 1
	d = x + 9
	a = c
	if (MAGIC_EQ(c, b)):
		a = 2 * c
	elif (MAGIC_EQ(a, c)):
		a = c + 5
		d = b + 10
	else:
		d = a + 4
	if (MAGIC_EQ(a, d)):
		raise("Oh no")
Banana(10)

