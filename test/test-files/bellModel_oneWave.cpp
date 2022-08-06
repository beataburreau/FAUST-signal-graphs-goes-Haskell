/* ------------------------------------------------------------
name: "bellModel"
Code generated with Faust 2.40.8 (https://faust.grame.fr)
Compilation options: -lang cpp -es 1 -mcd 16 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __mydsp_H__
#define  __mydsp_H__

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif 

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <math.h>

#ifndef FAUSTCLASS 
#define FAUSTCLASS mydsp
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif

#if defined(_WIN32)
#define RESTRICT __restrict
#else
#define RESTRICT __restrict__
#endif

static float mydsp_faustpower2_f(float value) {
	return value * value;
}

class mydsp : public dsp {
	
 private:
	
	int iVec0[2];
	int iVec1[2];
	int fSampleRate;
	float fConst2;
	float fConst3;
	float fRec2[2];
	float fRec0[2];
	
 public:
	
	void metadata(Meta* m) { 
		m->declare("basics.lib/name", "Faust Basic Element Library");
		m->declare("basics.lib/version", "0.6");
		m->declare("compile_options", "-lang cpp -es 1 -mcd 16 -single -ftz 0");
		m->declare("filename", "bellModel.dsp");
		m->declare("maths.lib/author", "GRAME");
		m->declare("maths.lib/copyright", "GRAME");
		m->declare("maths.lib/license", "LGPL with exception");
		m->declare("maths.lib/name", "Faust Math Library");
		m->declare("maths.lib/version", "2.5");
		m->declare("name", "bellModel");
		m->declare("platform.lib/name", "Generic Platform Library");
		m->declare("platform.lib/version", "0.2");
	}

	virtual int getNumInputs() {
		return 0;
	}
	virtual int getNumOutputs() {
		return 1;
	}
	
	static void classInit(int sample_rate) {
	}
	
	virtual void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		float fConst0 = std::min<float>(192000.0f, std::max<float>(1.0f, float(fSampleRate)));
		float fConst1 = std::pow(0.00100000005f, 0.100000001f / fConst0);
		fConst2 = std::cos(6283.18555f / fConst0) * (0.0f - 2.0f * fConst1);
		fConst3 = mydsp_faustpower2_f(fConst1);
	}
	
	virtual void instanceResetUserInterface() {
	}
	
	virtual void instanceClear() {
		for (int l0 = 0; l0 < 2; l0 = l0 + 1) {
			iVec0[l0] = 0;
		}
		for (int l1 = 0; l1 < 2; l1 = l1 + 1) {
			iVec1[l1] = 0;
		}
		for (int l2 = 0; l2 < 2; l2 = l2 + 1) {
			fRec2[l2] = 0.0f;
		}
		for (int l3 = 0; l3 < 2; l3 = l3 + 1) {
			fRec0[l3] = 0.0f;
		}
	}
	
	virtual void init(int sample_rate) {
		classInit(sample_rate);
		instanceInit(sample_rate);
	}
	virtual void instanceInit(int sample_rate) {
		instanceConstants(sample_rate);
		instanceResetUserInterface();
		instanceClear();
	}
	
	virtual mydsp* clone() {
		return new mydsp();
	}
	
	virtual int getSampleRate() {
		return fSampleRate;
	}
	
	virtual void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("bellModel");
		ui_interface->closeBox();
	}
	
	virtual void compute(int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		for (int i0 = 0; i0 < count; i0 = i0 + 1) {
			iVec0[0] = 1;
			iVec1[0] = 1 - iVec0[1];
			int iTemp0 = 1 - (iVec0[1] + iVec1[1]);
			fRec2[0] = float(iTemp0 * (iTemp0 > 0)) - (fConst2 * fRec2[1] + fConst3 * fRec0[1]);
			fRec0[0] = fRec2[1];
			float fRec1 = fRec2[0];
			output0[i0] = FAUSTFLOAT(fRec1 - fRec0[1]);
			iVec0[1] = iVec0[0];
			iVec1[1] = iVec1[0];
			fRec2[1] = fRec2[0];
			fRec0[1] = fRec0[0];
		}
	}

};

#endif
