# Model Overview
This folder is intended to contain the GCAM model files used in the analysis. However, due to the size and complexity of the GCAM model, the actual model files are not included in this repository. 

Instead, users should run the following command from the root directory of this repository to clone GCAM as a submodule:

```bash
git submodule update --init model/gcam-core
```

If you just want to check out the model files, you can view `gpk/paper/agu2024` branch of the GCAM repository here: https://github.com/pkyle/gcam-core/tree/gpk/paper/agu2024

For more information on setting up and using GCAM, please refer to the [GCAM wiki](http://jgcri.github.io/gcam-doc/toc.html).

# Folder Structure
Key folders within the `model/` directory are as follows:

```
model/
├── gcam-core/                # GCAM model (cloned as a submodule)
│   ├── cvs/                  # core model C++ files 
│   ├── exe/                  # model configuration and executable files
│   ├── input/                # input and gcamdata files with xmls
│   ├── output/               # output database
│   ├── ModelInterface/       # to view outputs in a GUI
```

# Experiment Info
The configuration file that sets up the GCAM model for this analysis is [`gcam-core/exe/configuration_agu_base.xml`](./gcam-core/exe/configuration_agu_base.xml).

Run GCAM with this configuration file using the following command from the `gcam-core/exe/` directory:

```bash
./gcam.exe -C configuration_agu_base.xml
```

The output database from running GCAM with this configuration is expected to be in the [`gcam-core/output/database_basexdb`](./gcam-core/output/database_basexdb) folder, which should contain all the scenario outputs needed for the analysis.