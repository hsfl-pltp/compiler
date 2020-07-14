
typedef struct {
    float value;
} ElmFloat;

ElmFloat* _Basics_newElmFloat(float value) {
    ElmFloat* p = (ElmFloat*)malloc(sizeof(ElmFloat));
    p->value = value;
    return p;
};

float _Basics_voidToFloat (void* pointer){
    return *((float *) pointer);
}

static void* _Basics_add(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia + ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_mul(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia * ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_div(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia / ib;
    return _Basics_newElmFloat(i);
}

static void* _Basics_sub(ElmFloat* n, ElmFloat* m) {
    ElmFloat* pa = n;
    ElmFloat* pb = m;
    float ia = pa->value;
    float ib = pb->value;
    float i = ia - ib;
    return _Basics_newElmFloat(i);
}

static void* _Debug_log(String n, void* m) {
    Serial.begin(9600);
    Serial.print(n);
    Serial.print(" ");
    Serial.println(_Basics_voidToFloat(m));
    return m;
}

void* _elm_core_Basics_False() {
		return Serial.print("Compiled in DEV mode.");
exit(EXIT_FAILURE);
}
void* _author_project_Test_main = (false) ? _Basics_newElmFloat(1) : _Basics_newElmFloat(0);

#include <stdlib.h> 

void setup() {
   
    Serial.begin(9600);
    Serial.println("Compiled in DEV mode.");

    //The Code here will only be executed once 
_author_project_Test_main()
}

void loop() {
  // put your main code here, to run repeatedly:

}