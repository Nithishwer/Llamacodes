%reload_ext autoreload
%autoreload 2
%matplotlib inline

from fastai.vision import *
from fastai.metrics import error_rate

# Defining the batch size for Network training

bs = 64

# Downloading the Oxford IIIT Pet databse

path = untar_data(URLs.PETS);
#path
#path.ls()
path_anno = path/'annotations'
path_img = path/'images'
fnames = get_image_files(path_img)
#fnames[:5]
np.random.seed(2)
pat = r'/([^/]+)_\d+.jpg$'

# Extracting file names

data = ImageDataBunch.from_name_re(path_img, fnames, pat, ds_tfms=get_transforms(), size=224, bs=bs).normalize(imagenet_stats)
#data.show_batch(rows=3, figsize=(7,6))
#print(data.classes)
#len(data.classes),data.c

# Defining and training CNN

learn = cnn_learner(data, models.resnet34, metrics=error_rate)
learn.model
learn.fit_one_cycle(4)
learn.save('stage-1')

# Interpreting results

interp = ClassificationInterpretation.from_learner(learn) # The learn object has both the data and the trained models
losses,idxs = interp.top_losses()
len(data.valid_ds)==len(losses)==len(idxs)
interp.plot_top_losses(9, figsize=(15,11))


doc(interp.plot_top_losses)

#The above code with doc does not work due to a breaking change in the nbconvert package in version 5.5 (which is used in fastai lib)
#A quick workaround is to go back to version 5.4.1:
#conda install nbconvert=5.4.1
#Or just visit https://forums.fast.ai/t/doc-function-was-working-fine-yesterday-wont-anymore-on-colab/44929/10

interp.plot_confusion_matrix(figsize=(12,12), dpi=60)
interp.most_confused(min_val=2)

# Fine tuning the Model

learn.unfreeze()
learn.fit_one_cycle(1)

#Load previously trained model

learn.load('stage-1');
learn.lr_find()
learn.recorder.plot()
learn.unfreeze()
learn.fit_one_cycle(2, max_lr=slice(1e-6,1e-4))

# Using Resnet with 50 layers

data = ImageDataBunch.from_name_re(path_img, fnames, pat, ds_tfms=get_transforms(),size=299, bs=bs//2).normalize(imagenet_stats)
learn = cnn_learner(data, models.resnet50, metrics=error_rate)
learn.lr_find()
learn.recorder.plot()
learn.fit_one_cycle(8)
learn.save('stage-1-50')
learn.unfreeze()
learn.fit_one_cycle(3, max_lr=slice(1e-6,1e-4))
learn.load('stage-1-50');

# Interpreting results again

interp = ClassificationInterpretation.from_learner(learn)
interp.most_confused(min_val=2)
