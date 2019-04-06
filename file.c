extern void __VERIFIER_error() __attribute__ ((__noreturn__));
unsigned int __VERIFIER_nondet_uint();
int main() {
int C1Num = 1; 
int C1=1;
int B1Num = 0; 
int B1=__VERIFIER_nondet_int();
int A1Num = 1; 
int A1=__VERIFIER_nondet_int();
int if1=0;
if(C1!= 0) { 
	if1=A1; 
	if1Num =A1Num; 
} else { 
	if1Num =B1Num; 
	if1=B1; 
}
int A2Num = 1; 
int A2=if1;
if(A1Num == 0) 
	 __VERIFIER_error(); 
if(A2Num == 0) 
	 __VERIFIER_error(); 
int B2Num = 1; 
int B2=A1+A2;

}