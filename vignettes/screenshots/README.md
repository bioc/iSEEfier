# Vignette screenshots

This folder contains the screenshots of the app, generated in a pseudo-automated manner - one that does not involve too much of cropping, resizing, and customizing resolution.

The way it works:

```
app <- iSEE(sce, initial)   # as in the vignette chunk 
```

Once you created the `app` object, call `appshot()` from the `webshot2` package

```
webshot2::appshot(app, file = "screenshots/app_snapped.png", delay = 20)
```

The use of the delay parameter is to ensure that the app has loaded all the panels correctly.

Then, in the vignette, we simply use `knitr::include_graphics()` to embed the images as we normally would.
