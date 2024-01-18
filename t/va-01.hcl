defun fn-y (t1 t2 va-ctx) {
        | i |
	i := 0;
        while (< i (va-count va-ctx)) {
                printf "fn-y=>Y-VA[%d]=>[%d]\n" i (va-get i va-ctx);
		i := (+ i 1);
        };
};

defun x(a b ... ::: x y z) {
        |i|

        set x (va-count);
        set y (* a b);
        set z (+ a b);

        set i 0;
        while (< i (va-count)) {
                printf "VA[%d]=>[%d]\n" i (va-get i);
                set i (+ i 1);
        };
        fn-y "hello" "world" (va-context);

        return;
};

set t (x 10 20 30);
if (/= t 1) {
	printf "ERROR: t is not 1\n"
} else {
	printf "OK: %d\n" t
};

set t (set-r a b c (x 10 20 30 40 50));
if (/= t 3) {
	printf "ERROR: t is not 3\n"
} else {
	printf "OK: %d\n" t
};
if (/= a 3) {
	printf "ERROR: a is not 3\n"
} else {
	printf "OK: %d\n" a
};
if (/= b 200) {
	printf "ERROR: b is not 200\n"
} else {
	printf "OK: %d\n" b
};
if (/= c 30) {
	printf "ERROR: c is not 30\n"
} else {
	printf "OK: %d\n" c
};
