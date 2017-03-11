library(EBImage)

#' @import EBImage 
#' @import methods 
#' @import stats
NULL

#' A sample segment of an organoid
#' @name cells
#' @docType data
#' @author Jan Sauer
#' @keywords data
NULL

#' MaxContrastProjection: A package for performing z-projections of image 
#' stacks
#'
#' The package MaxContrastProjection provides functions to perform the 
#' common intensity projections (max, min, etc.) as well as a maximum 
#' contrast projection we introduce here.
#'
#' @docType package
#' @name MaxContrastProjection
#' @examples 
#' data(cells)
#' proj = contrastProjection(imageStack = cells, w_x = 15, smoothing = 5)
NULL

#' @title Validate Function Arguments
#' 
#' @description Validate the function arguments based on predefined rules. This
#' function has no application for users and serves only to ensure correct 
#' variables. It either returns TRUE if all variables conform to the rules or 
#' stops execution in case one of the conditions is not met.
#' 
#' @param imageStack A numeric 3D array-like
#' @param image A 2D array-like
#' @param w_x A numeric value
#' @param w_y A numeric value
#' @param smoothing An integer value
#' @param brushShape A string
#' @param projType A string
#' @param interpolation An integer value
#' @param contrastStack A numeric 3D array-like
#' @param indexMap A numeric 2D array-like. Must always be validated 
#' simultaneously with \code{imageStack}
#' @param fix.gaussian.blur A logical parameter, or one which can be coerced 
#' to a logical value
#' @param blur.size An integer value
#' @param return.all A logical parameter, or one which can be coerced to a 
#' logical value
#' 
#' @usage
#' validateVariables(imageStack, image, w_x, w_y, smoothing, brushShape, 
#'     projType, interpolation, contrastStack, indexMap, fix.gaussian.blur, 
#'     blur.size, return.all)
#' 
#' @return A boolean indicating that the function ran without error.
#' 
#' @author Jan Sauer
#' @keywords array
#' 
#' @examples print(validateVariables)
#' @export
validateVariables = function(imageStack, image, w_x, w_y, smoothing, 
                            brushShape, projType, interpolation, 
                            contrastStack, indexMap, fix.gaussian.blur, 
                            blur.size, return.all) {
    BRUSH_SHAPES = c("box", "disc")
    PROJ_TYPES = c("max", "min", "mean", "median", "sd", "sum")
    
    if(!missing(imageStack)) {
        if(!is.numeric(imageStack)) stop("'imageStack' must be numeric")
        if(length(dim(imageStack)) != 3) 
            stop("'imageStack' must be a 3D array")
        if(min(imageStack) < 0) stop("All values of 'imageStack' must be >= 0")
    }
    
    if(!missing(contrastStack)) {
        if(!is.numeric(contrastStack)) stop("'contrastStack' must be numeric")
        if(length(dim(contrastStack)) != 3) 
            stop("'contrastStack' must be a 3D array")
        if(min(contrastStack) < 0) 
            stop("All values of 'contrastStack' must be >= 0")
    }
    
    if(!missing(w_x)) {
        if(!is.numeric(w_x)) stop("'w_x' must be numeric")
        if(w_x < 0) stop("'w_x' must be positive")
    }
    
    if(!missing(smoothing)) {
        if(!is.numeric(smoothing)) stop("'smoothing' must be an integer")
        if(as.integer(smoothing) != smoothing) 
            stop("'smoothing' must be an integer")
        if(smoothing < 0) stop("'smoothing' must be >= 0")
    }
    
    if(!missing(interpolation)) {
        if(!is.numeric(interpolation)) 
            stop("'interpolation' must be an integer")
        if(as.integer(interpolation) != interpolation) 
            stop("'interpolation' must be an integer")
        if(interpolation < 0) stop("'interpolation' must be >= 0")
    }
    
    if(!missing(w_y)) {
        if(!is.numeric(w_y)) stop("'w_y' must be numeric")
    }
    
    if(!missing(image)) {
        if(!is.numeric(image)) stop("'image' must be numeric")
        if(length(dim(image)) != 2) stop("'image' must be a 2D array")
        if(min(image) < 0) stop("All values of 'image' must be >= 0")
    }
    
    if(!missing(brushShape)) {
        if(!brushShape %in% BRUSH_SHAPES) 
            stop("'brushShape' must be one of: ", 
                paste(BRUSH_SHAPES, collapse=", "))
    }
    
    if(!missing(projType)) {
        if(!projType %in% PROJ_TYPES)
            stop("'projType' must be one of: ", 
                paste(PROJ_TYPES, collapse = ", "))
    }
    
    if(!missing(indexMap)) {
        if(missing(imageStack)) 
            stop(strwrap("Validation of 'indexMap' only makes sense if 
                        'imageStack' is also being validated"))
        if(!is.numeric(indexMap)) stop("'indexMap' must be numeric")
        if(length(dim(indexMap)) != 2) 
            stop("'indexMap' must have two dimensions: (height, width)")
        if(!all(dim(imageStack)[c(1,2)] == dim(indexMap))) 
            stop(strwrap("The spatial dimensions of 'imageStack' and 'indexMap'
                        must be identical"))
        if(min(indexMap) < 1) stop("All values of 'indexMap' must be >= 1")
        if(max(indexMap) > dim(imageStack)[3]) 
            stop(strwrap("All values of 'indexMap' must be <= the number of 
                        image stacks (", dim(imageStack)[3], ")"))
    }
    
    if(!missing(fix.gaussian.blur)) {
        fix.gaussian.blur.log = as.logical(fix.gaussian.blur)
        if(is.na(fix.gaussian.blur.log)) 
            stop(strwrap("'fix.gaussian.blur' must be a TRUE/FALSE or must be 
                        able to be coerced to TRUE/FALSE"))
    }
    
    if(!missing(blur.size)) {
        if(!is.numeric(blur.size)) stop("'blur.size' must be an integer")
        if(as.integer(blur.size) != blur.size)
            stop("'blur.size' must be an integer")
        if(blur.size < 0) stop("'blur.size' must be >= 0")
    }
    
    if(!missing(return.all)) {
        return.all.log = as.logical(return.all)
        if(is.na(return.all.log)) 
            stop(strwrap("'return.all' must be a TRUE/FALSE or must be able to 
                        be coerced to TRUE/FALSE"))
    }
    
    return(TRUE)
}

#' @title Calculate Local Image Contrast
#' 
#' @description Calculate the local contrast of an input image for every pixel 
#' of the image over a given window centered on that pixel.
#' 
#' @param image A numeric 2D matrix-like on which the contrast should be 
#' determined
#' @param w_x The size of the window in x-direction
#' @param w_y The size of the window in y-direction
#' @param brushShape A string indicating the shape of the window. Currently 
#' supported values are \code{"box", "disc"} and the default is "disc".
#' @param validate A boolean value indicating if the variables need to be 
#' validated or if this function is being called internally, i.e. the variables
#' have already been validated once. This is only used to marginally speed up 
#' internal calls to functions and has no bearing on the actual functionality 
#' of the method.
#' 
#' @details The local contrast is calculated for every pixel of \code{image}. 
#' This means that a window is centered around a given pixel and the variance 
#' of the intensity values within this window are determined via 
#' Var = E[X^2] - (E[X])^2.
#' 
#' The \code{brushShape} indicates the shape of the window over which to 
#' calculate the variance. Depending on the symmetry of the objects being 
#' imaged, the window shape may have a significant impact on the quality of 
#' the projection.
#' 
#' @return A 2D matrix with the same dimensions as \code{image}, which shows 
#' the local contrast at each corresponding pixel of \code{image}.
#' 
#' @author Jan Sauer
#' @keywords array
#' 
#' @examples print(calcContrast)
#' @export
calcContrast = function(image, w_x, w_y, brushShape="disc", validate=TRUE) {
    # Check that all necessary arguments are present
    if(missing(image)) stop("'image' is missing")
    if(missing(w_x)) stop("'w_x' is missing")
    if(missing(w_y)) stop("'w_y' is missing")
    
    # Transform image if it's an EBImage object
    if(class(image) == "Image") image = imageData(image)
    
    # Validate arguments
    if(validate) valid = validateVariables(image = image, w_x = w_x, w_y = w_y,
                                        brushShape = brushShape)
    
    f = NULL
    if(brushShape == "box") 
        f = matrix(1 / ((2*w_x + 1)*(2*w_y + 1)), 
                nrow = (2*w_y + 1), ncol = (2*w_x + 1))
    if(brushShape == "disc") {
        f = makeBrush(size = 2*w_x + 1, shape = "disc", step = TRUE)
        f = f / sum(f)
    }
    imgSquared = image**2
    contrast = filter2(x=Image(imgSquared), filter=f, boundary="replicate") - 
        filter2(x=Image(image), filter=f, boundary="replicate")**2
    return(contrast)
}

#' @describeIn contrastProjection Get the full stack of contrast maps for each 
#' image in the image stack
#' @export
getContrastStack = function(imageStack, w_x, w_y, brushShape="disc", 
                            validate=TRUE) {
    # Check that all necessary arguments are present
    if(missing(imageStack)) stop("'imageStack' is missing")
    if(missing(w_x)) stop("'w_x' is missing")
    if(missing(w_y)) stop("'w_y' is missing")
    
    # Transform imageStack if it's an EBImage object
    if(class(imageStack) == "Image") imageStack = imageData(imageStack)
    
    # Validate arguments
    if(validate) valid = validateVariables(imageStack=imageStack, w_x=w_x, 
                                        w_y=w_y, brushShape=brushShape)
    
    # Calculate the contrast for each image in the stack
    img_frames = getFrames(imageStack) 
    contrast_stack = lapply(img_frames, calcContrast, w_x, w_y, brushShape)
    contrast_stack = combine(contrast_stack)
    
    return(imageData(contrast_stack))
}

#' @describeIn contrastProjection Get the index map (with or without smoothing)
#' which indicates the layer corresponding to the highest contrast for each 
#' pixel in the \eqn{(x,y)}-plane
#' @export
getIndexMap = function(contrastStack, smoothing=0, validate=TRUE) {
    # Check that all necessary arguments are present
    if(missing(contrastStack)) stop("'contrastStack' is missing")
    if(missing(smoothing)) stop("'smoothing' is missing")
    
    # Validate arguments
    if(validate) valid = validateVariables(contrastStack=contrastStack, 
                                        smoothing=smoothing)
    
    index_map = apply(contrastStack, c(1,2), function(x) which(x == max(x))[1])
    if(smoothing > 0) {
        max_id = max(index_map)
        index_map = round(medianFilter(index_map / max_id, smoothing) * max_id)
    }
    
    return(index_map)
}

#' @title Maximum contrast projection of a 3D image stack
#' 
#' @description Projects a z-stack of 2D images according to the highest local
#' contrast. Optionally, median smoothing can be applied to the resulting
#' projection index map prior to the projection itself.
#' 
#' @param imageStack A numeric 3D array-like which should ne projected. The 
#' dimensions should be (spatial_1, spatial_2, numer_of_images)
#' @param w_x The size of the window in x-direction
#' @param w_y The size of the window in y-direction
#' @param smoothing The size of the median filter window. If this is 0, median 
#' smoothing is not applied.
#' @param brushShape A string indicating the shape of the window. Currently 
#' supported values are \code{"box", "disc"} and "disc" is the default
#' @param interpolation The size of the blurring kernel to use when 
#' interpolating values at the boundaries of regions in the index map. Set to 
#' 0 for no interpolation.
#' @param indexMap A custom index map according to which the image stack is 
#' projected. The values must be integers between 1 and the number of layers 
#' in \code{imageStack}
#' @param validate A boolean value indicating if the variables need to be 
#' validated or if this function is being called internally, i.e. the variables
#' have already been validated once. This is only used to marginally speed up 
#' internal calls to functions and has no bearing on the actual functionality 
#' of the method.
#' @param contrastStack A numeric 3D array-like which contains the local 
#' contrasts for each image in \code{imageStack} at each pixel
#' @param fix.gaussian.blur A logical value indicating whether the false 
#' gaussian blur caused by unfocused images should be fixed or not (see 
#' vignette for more details on this).
#' @param blur.size An integer indicating the radius of the gaussian blur. 
#' Ignored unless \code{fix.gaussian.blur = TRUE}. The total diameter of the 
#' brush is defined as 2*blur.size+1, meaning that a 'blur.size' value of 0 
#' will result in a 1x1 pixel brush.
#' @param return.all A logical value indicating whether only the projection 
#' should be returned (FALSE) or if all intermediate results should be returned
#' as well, including the index map and the contrast stack (TRUE)
#' 
#' @details The local contrast for every image in the stack is determined using
#' \code{calcContrast}. \code{getContrastStack} returns this stack of contrast 
#' maps. Then, the z-layer with the highest local contrast is determined for 
#' each pixel in the \eqn{(x,y)}-plane, resulting in an index map with the 
#' same spatial dimensions as the input images. This index map can then be 
#' smoothed with a median filter if desired. \code{getIndexMap} returns this 
#' index map. Lastly, the image stack is projected into the \eqn{(x,y)}-plane 
#' using this index map to determine which z-layer to use at every pixel.
#' \code{contrastProjection} returns this fully projected image.
#' 
#' The \code{brushShape} indicates the shape of the window over which to 
#' calculate the variance. Depending on the symmetry of the objects being 
#' imaged, the window shape may have a significant impact on the quality of 
#' the projection.
#' 
#' If an object lies in several focal plains then the projection may include 
#' some artifical boundaries at the edges of the regions in each focal plain. 
#' Linear interpolation between the two layers at their boundaries serves to 
#' eliminate this problem. The \code{interpolation} size gives the size of the 
#' kernel to use for blurring the boundaries between individual regions of the 
#' index map. The projection values at these boundaries are then interpolated 
#' based on the non-integer values on the index maps. For example, if a pixel 
#' on the index map has the value 7.25, then the projected value at this pixel 
#' is 75% of the intensity in z-layer 7 and 25% of the intensity in layer 8.
#' 
#' If a very bright object lies on a dark background, then the gaussian 
#' blurring of the unfocused image stacks can create a brighter ring structure 
#' around this object. Fixing this involves Voronoi propagation into the 
#' regions directly surrounding bright objects. This correction only makes 
#' sense if there is a clear differentiation between fore- and background 
#' in the image. A perfect segmentation is unnecessary as the rings will only 
#' appear around exceptionally bright objects, which are easy to segment.
#' 
#' @usage
#' contrastProjection(imageStack, w_x, w_y = NULL, smoothing = 0,
#'     brushShape = "disc", interpolation = 0, fix.gaussian.blur = FALSE,
#'     blur.size = 0, return.all = FALSE)
#' 
#' @return
#' \describe{
#'     \item{contrastProjection}{A 2D matrix corresponding to the maximum 
#'     contrast projection of \code{imageStack}}
#'     \item{getIndexMap}{A 2D matrix indicating the z-layer with the maximum 
#'     contrast at every pixel in the \eqn{(x,y)-plane} of \code{imageStack}}
#'     \item{getContrastStack}{a 3D array corresponding to the contrast map 
#'     for every image of \code{imageStack}}
#'     \item{projection_fromMap}{A 2D matrix corresponding to the maximum 
#'     contrast projection of \code{imageStack}}
#' } 
#'  
#' @author Jan Sauer
#' @keywords array
#' 
#' @examples 
#' print(contrastProjection)
#' print(getIndexMap)
#' print(getContrastStack)
#' @export
contrastProjection = function(imageStack, w_x, w_y=NULL, smoothing=0, 
                            brushShape="disc", interpolation=0, 
                            fix.gaussian.blur=FALSE, blur.size=0, 
                            return.all=FALSE) {
    # Check that all necessary arguments are present
    if(missing(imageStack)) stop("'imageStack' is missing")
    if(missing(w_x)) stop("'w_x' is missing")
    
    # If w_y isn't explicitly set, then it is equal to w_x
    if(is.null(w_y)) w_y = w_x
    
    # Transform imageStack if it's an EBImage object
    if(class(imageStack) == "Image") imageStack = imageData(imageStack)
    
    # Validate arguments
    valid = validateVariables(imageStack=imageStack, w_x=w_x, w_y=w_y, 
                            smoothing=smoothing, brushShape=brushShape, 
                            fix.gaussian.blur=fix.gaussian.blur, 
                            blur.size=blur.size, return.all=return.all)
    
    # Get the contrast stack
    contrast_stack = getContrastStack(imageStack=imageStack, w_x=w_x, w_y=w_y, 
                                    brushShape=brushShape, validate=FALSE)
    
    # Get the index map
    index_map = getIndexMap(contrastStack=contrast_stack, smoothing=smoothing, 
                            validate=FALSE)
    
    # Fix the false blur around bright objects if desired
    if(fix.gaussian.blur)
        index_map = fixGaussianBlur(imageStack=imageStack, indexMap=index_map, 
                                    blur.size=blur.size)
    
    # Project the stack according to the index map
    proj = projection_fromMap(imageStack=imageStack, indexMap=index_map, 
                            interpolation=interpolation)
    
    if(return.all == FALSE) return(proj)
    if(return.all == TRUE) return(list(Contrast.Stack=contrast_stack, 
                                    Index.Map=index_map, Projection=proj))
}

#' @describeIn contrastProjection Get the index map (with or without smoothing)
#' which indicates the layer corresponding to the highest contrast for each 
#' pixel in the \eqn{(x,y)}-plane
#' @export
projection_fromMap = function(imageStack, indexMap, interpolation=0, 
                            validate=TRUE) {
    # Check that all necessary arguments are present
    if(missing(imageStack)) stop("'imageStack' is missing")
    if(missing(indexMap)) stop("'indexMap' is missing")
    
    # Validate variables
    if(validate)
        valid = validateVariables(imageStack=imageStack, indexMap=indexMap, 
                                interpolation=interpolation)
    
    storage.mode(indexMap) = "integer"
    
    if(interpolation > 0) {
        # Blur edges according to interpolation size
        edge_filter = matrix(1 / interpolation**2, nrow=interpolation, 
                            ncol=interpolation)
        indexMap_blurred = filter2(x=indexMap, filter=edge_filter)
        # This eliminates machine precision issues. There is no deeper meaning 
        # to the number here. Change only if it causes problems.
        indexMap_blurred = round(indexMap_blurred, digits=5)
        
        # Get the low and high projection
        indexMap_blurred_low = matrix(floor(indexMap_blurred), 
                                    nrow=nrow(indexMap_blurred), 
                                    ncol=ncol(indexMap_blurred))
        selec_ind_low = expand.grid(seq_len(nrow(indexMap_blurred_low)), 
                                    seq_len(ncol(indexMap_blurred_low)))
        low_proj = matrix(imageStack[cbind(selec_ind_low$Var1, 
                                        selec_ind_low$Var2, 
                                        as.vector(indexMap_blurred_low))],
                        nrow(indexMap_blurred_low), 
                        ncol(indexMap_blurred_low))
        
        indexMap_blurred_high = matrix(ceiling(indexMap_blurred), 
                                    nrow=nrow(indexMap_blurred), 
                                    ncol=ncol(indexMap_blurred))
        selec_ind_high = expand.grid(seq_len(nrow(indexMap_blurred_high)), 
                                    seq_len(ncol(indexMap_blurred_high)))
        high_proj = matrix(imageStack[cbind(selec_ind_high$Var1, 
                                            selec_ind_high$Var2, 
                                            as.vector(indexMap_blurred_high))],
                        nrow(indexMap_blurred_high), 
                        ncol(indexMap_blurred_high))
        
        high_proj_contribution = (indexMap_blurred - indexMap_blurred_low)
        proj = (high_proj_contribution * high_proj) + 
            ((1 - high_proj_contribution) * low_proj)
    } else {
        selec.ind = expand.grid(seq_len(nrow(indexMap)), 
                                seq_len(ncol(indexMap)))
        proj = matrix(imageStack[cbind(selec.ind$Var1, selec.ind$Var2, 
                                    as.vector(indexMap))], 
                    nrow(indexMap), ncol(indexMap))
    }
    
    return(proj)
}

#' @title Intensity Projection
#' 
#' @description Perform an intensity projection of a stack of images. The type 
#' of projection depends on the \code{projType}
#' 
#' @param imageStack A numeric 3D array-like which should ne projected. The 
#' dimensions should be (spatial_1, spatial_2, numer_of_images)
#' @param projType A string indicating the type of projection. Defaults to 
#' "max" and can take on the following values: 
#' \code{"max", "min", "mean", "median", "sd", "sum"}
#' 
#' @details The \code{projType} determines the time of projection to be used:
#' \describe{
#'     \item{max}{Each pixel of the output image takes on the maximum value of 
#'     the z-stack underneath the corresponding pixel of the input image 
#'     stack.}
#'     \item{min}{Each pixel of the output image takes on the minimum value of 
#'     the z-stack underneath the corresponding pixel of the input image 
#'     stack.}
#'     \item{mean}{Each pixel of the output image takes on the mean value of 
#'     the z-stack underneath the corresponding pixel of the input image 
#'     stack.}
#'     \item{median}{Each pixel of the output image takes on the median value 
#'     of the z-stack underneath the corresponding pixel of the input image 
#'     stack.}
#'     \item{sd}{Each pixel of the output image takes on the standard 
#'     deviation of the values of the z-stack underneath the corresponding 
#'     pixel of the input image stack.}
#'     \item{sum}{Each pixel of the output image takes on the sum of the 
#'     values of the z-stack underneath the corresponding pixel of the input 
#'     image stack.}
#' }
#' 
#' @return A 2D matrix corresponding to the maximum intensity projection of 
#' \code{imageStack}
#' 
#' @author Jan Sauer
#' @keywords array
#' 
#' @examples
#' dat = array(c(1,1,2,2,1,2,3,1), dim = c(2,2,2))
#' proj = intensityProjection(dat, projType = "max")
#' print(proj)
#' @export
intensityProjection = function(imageStack, projType="max") {
    # Check that all necessary arguments are present
    if(missing(imageStack)) stop("'imageStack' is missing")
    
    valid = validateVariables(imageStack=imageStack, projType=projType)
    
    proj = NULL
    proj = switch(projType,
                "max" = {apply(imageStack, c(1,2), max)},
                "min" = {apply(imageStack, c(1,2), min)},
                "mean" = {apply(imageStack, c(1,2), mean)},
                "median" = {apply(imageStack, c(1,2), median)},
                "sd" = {apply(imageStack, c(1,2), sd)},
                "sum" = {apply(imageStack, c(1,2), sum)},
                {stop("Invalid 'projType' value: ", projType)})
    return(proj)
}

#' @title Remove Gaussian Blur
#' 
#' @description Fix the contrast projection error around very bright objects on
#' a dark background
#' 
#' @param imageStack A numeric 3D array-like which should ne projected. The 
#' dimensions should be (spatial_1, spatial_2, numer_of_images)
#' @param indexMap A custom index map according to which the image stack is 
#' projected. The values must be integers between 1 and the number of layers 
#' in \code{imageStack}
#' @param blur.size An integer indicating the radius of the gaussian blur. The 
#' total diameter of the brush is defined as 2*blur.size+1, meaning that a 
#' blur.size' value of 0 will result in a 1x1 pixel brush.
#' @param validate A boolean indicating if the function arguments should be 
#' validated. This is only used to marginally speed up internal calls to 
#' functions and has no bearing on the actual functionality of the method.
#' 
#' @details Bright objects on dark backgrounds cause projection artefacts, a 
#' sort of gaussian "shadow" of the object. This is due to the unfocused images
#' having a higher  contrast in the regions directly outside of bright 
#' foreground objects than the actual background. This leads to ring shapes 
#' around the bright foreground which need to be removed. This is done by 
#' detecting bright objects with a primitive, intensity-based segmentation 
#' method (Otsu-thresholding) and determining a region around the foreground 
#' objects in which the gaussian blurring is visible. In this region, instead 
#' of using the determined z-indices for the projection, a Voronoi propagation 
#' starting from the closest foreground pixels is performed to ensure that the 
#' same z-layer is used for the nearby background as for the foreground.
#' 
#' @return An index map with the corrected values around the foreground objects
#' 
#' 
#' @author Jan Sauer
#' @keywords array
#' 
#' @examples 
#' print(fixGaussianBlur)
#' @export
fixGaussianBlur = function(imageStack, indexMap, blur.size, validate=TRUE) {
    if(missing(imageStack)) stop("'imageStack' is missing")
    if(missing(indexMap)) stop("'indexMap' is missing")
    
    if(validate) 
        valid = validateVariables(imageStack=imageStack, indexMap=indexMap, 
                                blur.size=blur.size)
    
    # Use the minimum intensity projection to segment the FG vs BG
    proj_min = intensityProjection(imageStack, "min")
    otsu_thresh = otsu(x=proj_min, range=range(proj_min))
    proj_min_bin = proj_min >= otsu_thresh
    
    # Fill the small holes in proj_min_bin
    proj_min_bin = fillHull(proj_min_bin * 1)
    
    # In order to fix the blur, propagation is only required into the areas 
    # immediately surrounding the foreground spots. This has the advantage of 
    # not affecting potentially dark regions not detected by the thresholding 
    # as foreground
    f = makeBrush(size=blur.size*2+1, shape="disc", step=TRUE)
    bin_img_overlay = filter2(x=proj_min_bin, filter=f, boundary=0)
    bin_img_overlay[bin_img_overlay > 1] = 1
    bin_img_overlay = bin_img_overlay > otsu(x=bin_img_overlay, 
                                            range=range(bin_img_overlay))
    expand_region = bin_img_overlay * (1-proj_min_bin)
    
    # Define the seed map, i.e. the regions from which to propagate
    seed_map = indexMap * proj_min_bin
    
    # lambda is a mixing parameter between the euclidean distance and the 
    # gradient of 'x' in 'EBImage::propagate()' when calculating the distance 
    # 'd' between two points. According to the documentation of this function, 
    # as lambda goes to infinity, 'd' tends to the euclidean distance, so a 
    # sufficiently large value of lambda should be used here.
    indexMap_adjusted = propagate(x=matrix(0, nrow=dim(imageStack)[1], 
                                        ncol=dim(imageStack)[2]), 
                                seeds=seed_map, lambda=1e6)
    
    # The true index map is now the combination of the original index map plus 
    # the propagated values in the expand regions
    true_index_map = indexMap
    true_index_map[expand_region == 1] = indexMap_adjusted[expand_region == 1]
    
    return(true_index_map)
}