extern void __VERIFIER_error() __attribute__ ((__noreturn__));
unsigned int __VERIFIER_nondet_uint();
int main() {
int C1=__VERIFIER_nondet_int();
int B1=__VERIFIER_nondet_int();
int if1=0;
if(B1) { 
	if1=C1; 
} else { 
	if1=__VERIFIER_nondet_int(); 
}
int A1=if1;
if(A1 == 0) 
	 __VERIFIER_error(); 
int A2=__VERIFIER_nondet_int()/A1;

}