/* 1.1 Newton Cooling Method */

%let h = 1; /* time step */
%let n = 300;
%let y_obs = 190; /* observed temp at time h_obs */
%let h_obs = 5;

%let c = %sysevalf((&y_obs - 200)/&h_obs)/(70 - 200);
%let a = %sysevalf(1 - &c*&h);
%let b = %sysevalf(&c*&h*70);

data work.fofd;
	retain t 0;
	retain y 200;
	/* Execute the FOFD Algorithm */
	do k = 1 to (&n - 1);
		y = &a*y + &b;
		t = t + &h;
		output;
	end;
run;

proc gplot data=work.fofd;
	plot y*t;
run;

/* 1.2 Heat Diffusion in a Wire */

%let l = 1; /* length of the wire */
%let t = 150; /* final time */
%let maxk = 30; /* number of time steps */
%let dt = %sysevalf(&t/&maxk);
%let n = 10; /* number of space steps */
%let dx = %sysevalf(&l/&n);
%let b = %sysevalf(&dt/(&dx*&dx));
%let cond = .001; /* thermal conductivity */
%let spheat = 1; /* specific heat */
%let rho = 1; /* density */
%let a = %sysevalf(&cond/(&spheat*&rho));
%let alpha = %sysevalf(&a*&b);
%let f = 1;

data measures (keep=timcol xxcol ucol);
	array xx{&n};
	array u{&n,&maxk};
	array tim{&maxk};

	do i = 1 to &n;   /* initial temperature */
		xx(i) =(i-1) * &dx;
		u(i,1) = sin(CONSTANT('PI') * xx(i));
	end;

	do k= 1 to &maxk;  /*  boundary temperature */
		u(1,k) = 0;
		u(&n,k) = 0;
		tim(k) = (k-1)*&dt;
	end;

	/* Execute the explicit method using nested loops. */
	do k=1 to &maxk - 1;    /* time loop */
	   do i=2 to &n - 1;   /*  space loop  */
			u(i,k+1) = &f * &dt / (&spheat * &rho) + (1 - 2 * &alpha) * u(i,k) + &alpha * (u(i-1,k) + u(i+1,k));
	   end;
	end;

	do k=1 to &maxk;
		timcol = tim(k);
		do i=1 to &n;
			xxcol = xx(i);
			ucol = u(i,k);
			output;
		end;
	end;
run;

/* Create the grid data */
/*proc g3grid data=measures out=a;*/
/*  grid xxcol*timcol = ucol / */
/*    axis1=0 to 1 by .1*/
/*    axis2=0 to 150 by 1;*/
/*run;*/

/* Plot the Surface */
proc g3d data=measures;
  plot xxcol*timcol = ucol;
run;

/* 1.6 Convergence Analysis */
/* Euler and Improved Euler Method for discretion errors */
/* Euler’s and Heun’s Methods for */
/* predicting temperature as a function of time */
%let T  = 50;	*final time;
%let y0 = 200;	*initial temperature;
%let c  = .15;	*cooling factor;
%let y_a  = 70;	*surrounding temperature;

/* Creates data for approximations and error */
/* steps = number of time steps */
%macro euler_heun(steps);
    %let size = %sysevalf(&steps + 1);
    %let dt = %sysevalf(&T/&steps);

    data euler_heun_&steps(keep= time eul heun steps err_eul                                       err_heun);
        array tim{&size};
        array y_exact{&size};
        array y_eul{&size};
        array y_heun{&size};
        array err_eul_calc{&size};
        array err_heun_calc{&size};
        tim(1) = 0;
        y_exact(1) = &y0;
        y_eul(1) = &y0;
        y_heun(1) = &y0;

        do k = 1 to &steps;
            tim(k+1) = k * &dt;
	    /* Exact Solution */
            y_exact(k+1) = &y_a + (&y0 - &y_a)*exp(-&c * k * &dt);

            /* Euler's method's approximation */
	    y_eul(k+1) = y_eul(k) + &dt * &c * (&y_a - y_eul(k));

	    /* Heun's method's approximation */
	    y_temp = y_heun(k) + &dt * &c * (&y_a - y_heun(k));
	    y_heun(k+1) = y_heun(k) + &dt/2*(&c*(&y_a-y_heun(k)) 
                      + &c * (&y_a - y_temp));

	    /* Errors */
	    err_eul_calc(k+1) = abs(y_eul(k+1) - y_exact(k+1));
	    err_heun_calc(k+1) = abs(y_heun(k+1) - y_exact(k+1));
        end;

        do k = 1 to &size;
            time = tim(k);
	    eul = y_eul(k);
	    heun = y_heun(k);
	    err_eul = err_eul_calc(k);
	    err_heun = err_heun_calc(k);
	    steps = &steps;
	    output;
	end;
    run;

    data errors_&steps(keep=steps err_eul err_heun time);
        set euler_heun_&steps;
	by steps;
	if time ^= 0 then output;
    run;
%mend;

%euler_heun(5);
%euler_heun(10);
%euler_heun(20);
%euler_heun(50);

data approxs;
    set euler_heun_5 euler_heun_10 euler_heun_20 euler_heun_50;
    label time="Time" eul="Euler Approximation" 
        heun="Heun Approximation" steps="Time Steps" 
        err_eul="Euler Error" err_heun="Heun Error";
run;

data errors(keep=steps err_eul err_heun time);
    set errors_5 errors_10 errors_20 errors_50;
    label steps="Time Steps" err_eul="Euler Error" 
          err_heun="Heun Errors" time="Time";
run;

symbol1 color=red interpol=join line=1;
symbol2 color=blue interpol=join line=1;
symbol3 color=green interpol=join line=1;
symbol4 color=orange interpol=join line=1;

proc gplot data=approxs;
    plot eul*time=steps;
run;

proc gplot data=approxs;
    plot heun*time=steps;
run;

proc print data=errors;
    by steps;
    var time err_eul err_heun;
run;


/* Approximate pi with Monte Carlo */
%let n = 1000;
data a;
	do i = 1 to &n;
		x = 2*rand("Uniform") - 1; 
		y = 2*rand("Uniform") - 1; 
		if x**2 + y**2 <= 1 then do;
			inner + 1;
			count + 1;
			eofxsq + 1**2;
		end;
		else outer + 1;

		pi_approx = 4*(inner/(outer+inner));
		err =  constant('PI') - pi_approx;

		eofxsq = eofxsq/&n;
		varx = eofxsq - (inner/&n)**2;
		sigx = sqrt(varx);
		sigma = 4*sigx/(sqrt(&n));
		output;
	end;
run;
	
proc gplot data=a;
	plot pi_approx*i /
	vref = 3.14159265359;
	where i>10;
run;

symbol1 interpol=join;

proc gplot data=a;
	plot x*y=incircle /
	vaxis=axis1 haxis=axis2;
	
run;

data a;
	do i = 1 to 100000;
		num = rand('normal');
		output;
	end;
run;
data b;
	do i = 1 to 100000;
		num = rand('exponential');
		output;
	end;
run;
data c;
	do i = 1 to 100000;
		num = rand('gamma', 7.25);
		output;
	end;
run;
title "Normal Distribution";
proc univariate data=a;
	histogram num / normal;
run;
title1 "Exponential Distribution";
proc univariate data=b;
	histogram num / exponential;
run;
title1 "Gamma Distribution";
proc univariate data=c;
	histogram num / gamma;
run;
