
# The gradient is the vector of partial derivatives.
# For functions like ours, the gradient gives the input direction
# in which the function most quickly increases.
# Thus one approach to maximizing a function is to pick a random starting point,
# computer the gradient, take a small step in the firection of the gradient, and repeat
# with the new starting point. Alternatively, you can minimize the function by taking small 
# steps in the opposite direction.

def step(v,direction,step_size):
	""" Move step_size in direction from v """
	return([v_i + step_size*direction_i for v_i, direction_i in zip(v,direction)])

def safe_fcn(fcn):
	try: 		return(fcn(*args,**kwargs))		#If our function diverges
	except:		return(float('inf'))			#Infinity

def minimize_function(target_fcn, gradient_fcn, theta0, tolerance=.00001):
	step_sizes = [100,10,1,.1,.01,.001,.0001,.00001]
	theta = theta0										#Set theta to initial value

	target_fcn = safe_fcn(target_fcn)				#Safe version of function

	value = target_fcn(theta)							#Value we're minimizing

	while True:
		gradient = gradient_fcn(theta)
		next_thetas = [step(theta, gradient, -step_size) for step_size in step_sizes]
		
		#chose the theta that minimizes the target function
		next_theta = min(next_thetas, key=target_fn)
		next_value = target_fcn(next_theta)

		#stop if we're converiging
		if(abs(value - next_value) < tolerance):
			return(theta)
		else:
			theta,value = next_theta,next_value

def negate(f):
	return(lambda *args, **kwargs: -f(*args, **kwargs))

def negate_all(f):
	return(lambda *args, **kwargs: [-y for y in f(*args, **kwargs)])

def maximize_function(target_fcn, gradient_fcn, theta0, tolerance=.00001):
	return(minimize_function(negate(target_fcn), negate_all(gradient_fcn), theta0, tolerance))

def f(x):
	return(x**2)

def g(x):
	return(sum(x))

print(minimize_function(f,g,-2))