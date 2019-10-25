def MAGIC_EQ(x, y):
	return x == y

def Banana(c):
	b = c + 7
	d = 10
	a = c
	if (MAGIC_EQ(c, b)):
		a = 2 * c
	elif (MAGIC_EQ(d, c)):
		a = c + 5
	else:
		a = b + 10
	if (MAGIC_EQ(a, d)):
		raise("Oh no")
Banana(10)

