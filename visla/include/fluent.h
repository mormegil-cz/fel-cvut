
#ifndef _fluent_parser_h_
#define _fluent_parser_h_

/* todo - predelat tyto #define s operatorem & na pole, ktere se bude predavat exportnimu volani */
#define FW_nodenumber 1
#define FW_x_coordinate 2
#define FW_y_coordinate 4
#define FW_z_coordinate 8
#define FW_x_velocity 16
#define FW_y_velocity 32
#define FW_z_velocity 64
#define FW_total_temperature 128
#define FW_all 0x7FFFFFFF


char *Tfluentattrstr [];

//#include "vector.h"

#include <iostream>
#include <string>
using namespace std;
#include <fstream>

class Tfluentnode {
public:
	float px, py, pz;
	float *values;

	/*
public:
	node (float x, float y, int items);
	node (int items);
	void setitem (int item, float value);
	node *next;
	*/
};

/*
node::node (float nx, float ny, int items)
{
	x = nx; y = ny;
	values = new float [items];
}

node::node (int items)
{
	values = new float [items];
}

void node::setitem (int item, float value)
{
	values [item] = value;
}
*/

class Tfluentattr {
public:
	string name;
        Tfluentattr () {};
//	~Tfluentattr (void);
        Tfluentattr(const char*name): name(name) {}
	int valueoffset;
};

class Tfluentvalue {
public:
	int offset; // prekladovy offset
//	string name; // bude odstraneno v dalsi verzi
	float min, max, avg;
};

class Tfluentparser {
protected:
	float dxmax, dymax, dzmax; // maximalni hodnoty rozmeru
	float dxmin, dymin, dzmin; // minimalni hodnoty rozmeru
	int maxnode, approxmaxnode;
	Tfluentnode *nodes;
	int maxvalue;
	int dim;
	Tfluentvalue values [256];
public:
	enum Tfluentattrenum {
		UNKNOWN,
		nodenumber, 
		x_coordinate, 
		y_coordinate, 
		z_coordinate, 
		pressure, 
		pressure_coefficient, 
		dynamic_pressure, 
		absolute_pressure, 
		total_pressure, 
		rel_total_pressure, 
		density, 
		density_all, 
		velocity_magnitude, 
		x_velocity, 
		y_velocity, 
		z_velocity,
		stream_function, 
		radial_velocity, 
		tangential_velocity, 
		rel_velocity_magnitude, 
		relative_x_velocity, 
		relative_y_velocity, 
		relative_z_velocity, 
		rel_tangential_velocity, 
		grid_x_velocity, 
		grid_y_velocity, 
		grid_z_velocity, 
		velocity_angle, 
		relative_velocity_angle, 
		vorticity_mag, 
		cell_reynolds_number, 
		temperature, 
		total_temperature, 
		enthalpy, 
		rel_total_temperature, 
		rothalpy, 
		wall_temp_out_surf, 
		wall_temp_in_surf, 
		total_enthalpy, 
		total_enthalpy_deviation, 
		entropy, 
		total_energy, 
		internal_energy, 
		absorption_coefficient, 
		scattering_coefficient, 
		radiation_temperature, 
		incident_radiation, 
		turb_kinetic_energy, 
		turb_intensity, 
		turb_diss_rate, 
		production_of_k, 
		viscosity_turb, 
		viscosity_eff, 
		viscosity_ratio, 
		thermal_conductivity_eff, 
		prandtl_number_eff, 
		y_star, 
		y_plus, 
		c__s__, 
		h2, 
		o2, 
		co2, 
		h2o, 
		n2, 
		c, 
		h, 
		o, 
		molef_c__s__, 
		molef_h2, 
		molef_o2, 
		molef_co2, 
		molef_h2o, 
		molef_n2, 
		molef_c, 
		molef_h, 
		molef_o, 
		concentration_c__s__, 
		concentration_h2, 
		concentration_o2, 
		concentration_co2, 
		concentration_h2o, 
		concentration_n2, 
		concentration_c, 
		concentration_h, 
		concentration_o, 
		relative_humidity, 
		fmean, 
		fmean2, 
		fvar, 
		fvar2, 
		fvar_prod, 
		fvar2_prod, 
		dpm_mass_source, 
		dpm_x_mom_source, 
		dpm_y_mom_source, 
		dpm_z_mom_source, 
		dpm_sensible_enthalpy_source, 
		dpm_total_enthalpy_source, 
		dpm_absorption_coefficient, 
		dpm_emission, 
		dpm_scattering, 
		dpm_burnout, 
		dpm_devolatization, 
		dpm_concentration, 
		viscosity_lam, 
		thermal_conductivity_lam, 
		specific_heat_cp, 
		prandtl_number_lam, 
		mean_molecular_weight, 
		wall_shear, 
		x_wall_shear, 
		y_wall_shear, 
		z_wall_shear, 
		skin_friction_coef, 
		heat_flux, 
		rad_heat_flux, 
		heat_transfer_coef, 
		nusselt_number, 
		stanton_number, 
		cell_partition, 
		cell_element_type, 
		cell_type, 
		cell_zone, 
		partition_neighbors, 
		axial_coordinate, 
		radial_coordinate, 
		x_surface_area, 
		y_surface_area, 
		z_surface_area, 
		x_face_area, 
		y_face_area, 
		z_face_area,
		cell_equiangle_skew, 
		cell_equivolume_skew, 
		cell_volume, 
		cell_wall_distance, 
		face_handedness, 
		face_squish_index, 
		cell_squish_index, 
		adaption_function, 
		existing_value, 
		boundary_cell_dist, 
		boundary_normal_dist, 
		boundary_volume_dist, 
		cell_volume_change, 
		cell_surface_area, 
		cell_warp, 
		cell_children, 
		cell_refine_level, 
		mass_imbalance, 
		strain_rate_mag, 
		dx_velocity_dx, 
		dy_velocity_dx, 
		dz_velocity_dx,
		dx_velocity_dy, 
		dy_velocity_dy, 
		dz_velocity_dy,
		dx_velocity_dz, 
		dy_velocity_dz, 
		dz_velocity_dz,
		dp_dx, 
		dp_dy, 
		dp_dz
	};
        int getvaluecount() { return maxvalue; }
	int getvalueoffset (int index);
	int manualvelocitylevel;
	string getvaluename (int index);
	Tfluentvalue *getvalueptr (int index);
	Tfluentattr **attr;
	string fluentfilename;
	Tfluentparser (string &fileName);
	float getavgvalue (int value);
	void displayvalue (int valueindex);
	void displayvalues (void);
	void displayinfo (bool brief=true);
	int findoffset (string value);
	~Tfluentparser (void);
	void addattr (string attr);
	void deletenodes (void);
};

class Tfluentdsparser : public Tfluentparser {
	int getpointer (int x, int y, int z);
	int dssx, dssy, dssz, dss; // dimenze bunek - napr. 24 x 32 x 1
//	node *dsnodes;
	float **dsvalues;
	int *dscounts;
public:
	void downsample (int ssx, int ssy, int ssz);
	float getdsvalue (int x, int y, int z, int value);
	float getdsvaluebyindex (int x, int y, int z, int index);
	float getdsvelocity (int x, int y, int z);
	Tfluentdsparser (string &fileName, int dsx, int dsy, int dsz, float minsx=-1, float minsy=-1, float minsz=-1, float maxsx=-1, float maxsy=-1, float maxsz=-1);
	~Tfluentdsparser (void);
	virtual void displayinfo (bool brief=true);
};

#endif
