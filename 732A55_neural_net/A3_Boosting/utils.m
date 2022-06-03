import numpy as np


def GenerateHaarFeatureMasks(nbrHaarFeatures):
    """ GenerateHaarFeatureMasks
    Generate a random set of filter masks for Haar feature extraction.
    The masks will be of random sizes (within certain limits), at random
    locations, and of random direction (aligned along x or y).
    Both first order ([-1 1]-type) and second order features
    ([-1 2 -1]-type) are generated.

    The default size of 24x24 is assumed (can easily be changed below).

    Input:
        nbrHaarFeatures - Number of Haar feature masks to generate

    Output:
        haarFeatureMasks - A [24 x 24 x nbrHaarFeatures] matrix with
                           the Haar filter masks

    Written by Ola Friman, 2012
    """

    # We assume that the image size is 24x24 to reduce the
    # number of input parameters.
    imgSizex = 24    # Equal to number of columns
    imgSizey = 24    # Equal to number of rows

    # Intialize mask matrix
    haarFeatureMasks = np.zeros((imgSizey,imgSizex,nbrHaarFeatures))

    # Create feature masks one at the time
    for k in range(nbrHaarFeatures):

        # Randomize 1:st or 2:nd derivative type of Haar feature
        featureType = np.random.choice(2)

        # Randomize direction (x or y) of the filter
        featureDirection = np.random.choice(2)

        if featureType == 0:   # 1:st derivative type
            # Size of one field of the feature. For the 1:st deriviative
            # type, there are 2 fields, i.e., the actual size is twice as big
            xSize = 2 + np.random.choice(8)
            ySize = 2 + np.random.choice(8)  # Size between 2x(2 and 9)

            #
            # Find random origin so that the feature fits within the image
            xOriginMax = imgSizex - 2*xSize
            yOriginMax = imgSizey - 2*ySize
            xOrigin = np.random.choice(xOriginMax) # TODO:This might not need to start at 1, but 0
            yOrigin = np.random.choice(yOriginMax)

            # Generate feature
            if featureDirection == 0:   # x-direction
                haarFeatureMasks[yOrigin:yOrigin+2*ySize-1, xOrigin:xOrigin+xSize-1,k] = -1
                haarFeatureMasks[yOrigin:yOrigin+2*ySize-1, xOrigin+xSize:xOrigin+2*xSize-1,k] = 1
            else:                       # y-direction
                haarFeatureMasks[yOrigin:yOrigin+ySize-1         ,xOrigin:xOrigin+2*xSize-1,k] = -1
                haarFeatureMasks[yOrigin+ySize:yOrigin+2*ySize-1 ,xOrigin:xOrigin+2*xSize-1,k] = 1

        elif featureType == 1:   # 2:nd derivative type
            # Size of one field of the feature. For the 1:st deriviative
            # type, there are 2 fields, i.e., the actual size is twice as big
            xSize = 2 + np.random.choice(5)
            ySize = 2 + np.random.choice(5)   # Size between 3x(2 and 6)

            # Find random origin so that the feature fits within the image
            xOriginMax = imgSizex - 3*xSize
            yOriginMax = imgSizey - 3*ySize
            xOrigin = np.random.choice(xOriginMax)
            yOrigin = np.random.choice(yOriginMax)

            # Generate feature
            if featureDirection == 0:   # x-direction
                haarFeatureMasks[yOrigin:yOrigin+3*ySize-1, xOrigin:xOrigin+xSize-1,k] = -1
                haarFeatureMasks[yOrigin:yOrigin+3*ySize-1, xOrigin+xSize:xOrigin+2*xSize-1,k] = 2
                haarFeatureMasks[yOrigin:yOrigin+3*ySize-1, xOrigin+2*xSize:xOrigin+3*xSize-1,k] = -1
            else:                       # y-direction
                haarFeatureMasks[yOrigin:yOrigin+ySize-1           ,xOrigin:xOrigin+3*xSize-1,k] = -1
                haarFeatureMasks[yOrigin+ySize:yOrigin+2*ySize-1   ,xOrigin:xOrigin+3*xSize-1,k] = 2
                haarFeatureMasks[yOrigin+2*ySize:yOrigin+3*ySize-1 ,xOrigin:xOrigin+3*xSize-1,k] = -1

    return haarFeatureMasks


def ExtractHaarFeatures(images,haarFeatureMasks):
    """ ExtractHaarFeatures
    Applies a number of Haar features from a stack of images.
    Input:
        images - A stack of images saved in a 3D matrix, first
                 image in image(:,:,1), second in image(:,:,2) etc.

        haarFeatureMasks - A stack of Haar feature filter masks saved in a 3D
                           matrix in the same way as the images. The haarFeatureMasks matrix is
                           typically obtained using the GenerateHaarFeatureMasks()-function

    Output:
        x - A feature matrix of size [nbrHaarFeatures,nbrOfImages] in which
            column k contains the result obtained when applying each Haar feature
            filter to image k.

    Written by Ola Friman, 2012
    """

    # Check that images and Haar filters have the same size
    if not images.shape[:2] == haarFeatureMasks.shape[:2]:
        raise Exception('Input image sizes do not match!')

    # Get number of images and number of features to extract
    nbrHaarFeatures = haarFeatureMasks.shape[2]
    nbrTrainingExamples = images.shape[2]

    # # Initialize matrix with feature values
    # x = np.zeros((nbrHaarFeatures,nbrTrainingExamples))

    # Extract features (using some Matlab magic to avoid one for-loop)
    x = haarFeatureMasks.reshape(-1,nbrHaarFeatures).T @ images.reshape(-1,nbrTrainingExamples)

    # NOTE, the above does the same as
    #for k = 1:nbrHaarFeatures
    #    for j = 1:nbrTrainingExamples
    #        x(k,j) = sum(sum(images(:,:,j).*haarFeatureMasks(:,:,k)));
    #    end
    #end

    return x

