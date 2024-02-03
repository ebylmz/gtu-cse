## Build

Package the project
```bash
pip install --editable .
```

Create virtual environment
```bash
conda create -n nlp python=3.7
```

Activate virtual environment
```bash
conda activate nlp
```

Install openfst and jupyter
```bash
pip install openfst-python jupyter
```

Create a python kernel which will work on this environment. 
```bash
python -m ipykernel install --user --name=nlp
```

Launch the jupyter lab and select the python kernel as "nlp" 
```bash
jupyter lab
```

To deactivate the virtual environment 
```bash
conda deactivate
```

To remove conda environment
```bash
conda env remove --name nlp
```


List all the kernels
```bash
jupyter kernelspec list
```

Uninstall your unwanted kernel
```bash
jupyter kernelspec uninstall nlp
```