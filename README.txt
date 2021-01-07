
This Fortran code was written by Kazumi Ozaki for examining the future life span of Earth's
oxygenated atmosphere. The results are published as Ozaki and Reinhard (2021).
The basic design of the model is based on the COPSE model that is originally developed by Tim
Lenton's group (Bergman et al., 2004 Am.J.Sci.; Lenton et al., 2016 PNAS). The reader should
consult these papers for a comprehensive explanation and empirical/theoretical basis. The model's 
framework was expanded for this study by including the global CH4 cycle and the interaction
with the mantle. The improvments are fully explained in the Methods section and the Supplementary
Material.

The model is written in modular form: Constants.f90 summarizes the constants used in the model, and
main.f90 is a main code. Input files (forcings) are located in the 'input' directory, and the output
files will be placed in the 'output' directory once the model is run. The each simulation will
take a few minnutes for complete on the desktop computer, but the Monte Carlo simulation will take
longer, depending on the sampling number. In this code, the number of sampling is controlled
by a variable of "resample" (see Constants.f90), which is set at 2,000. In Ozaki and Reinhard
(2011), we have run this code >200 times for the statistical analysis.