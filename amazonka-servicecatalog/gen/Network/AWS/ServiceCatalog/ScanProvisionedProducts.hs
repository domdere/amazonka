{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ScanProvisionedProducts
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the provisioned products that are available (not terminated).
--
--
module Network.AWS.ServiceCatalog.ScanProvisionedProducts
    (
    -- * Creating a Request
      scanProvisionedProducts
    , ScanProvisionedProducts
    -- * Request Lenses
    , sppAcceptLanguage
    , sppAccessLevelFilter
    , sppPageToken
    , sppPageSize

    -- * Destructuring the Response
    , scanProvisionedProductsResponse
    , ScanProvisionedProductsResponse
    -- * Response Lenses
    , spprsNextPageToken
    , spprsProvisionedProducts
    , spprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'scanProvisionedProducts' smart constructor.
data ScanProvisionedProducts = ScanProvisionedProducts'
  { _sppAcceptLanguage    :: !(Maybe Text)
  , _sppAccessLevelFilter :: !(Maybe AccessLevelFilter)
  , _sppPageToken         :: !(Maybe Text)
  , _sppPageSize          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScanProvisionedProducts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'sppAccessLevelFilter' - The access level to use to obtain results. The default is @User@ .
--
-- * 'sppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'sppPageSize' - The maximum number of items to return with this call.
scanProvisionedProducts
    :: ScanProvisionedProducts
scanProvisionedProducts =
  ScanProvisionedProducts'
  { _sppAcceptLanguage = Nothing
  , _sppAccessLevelFilter = Nothing
  , _sppPageToken = Nothing
  , _sppPageSize = Nothing
  }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
sppAcceptLanguage :: Lens' ScanProvisionedProducts (Maybe Text)
sppAcceptLanguage = lens _sppAcceptLanguage (\ s a -> s{_sppAcceptLanguage = a});

-- | The access level to use to obtain results. The default is @User@ .
sppAccessLevelFilter :: Lens' ScanProvisionedProducts (Maybe AccessLevelFilter)
sppAccessLevelFilter = lens _sppAccessLevelFilter (\ s a -> s{_sppAccessLevelFilter = a});

-- | The page token for the next set of results. To retrieve the first set of results, use null.
sppPageToken :: Lens' ScanProvisionedProducts (Maybe Text)
sppPageToken = lens _sppPageToken (\ s a -> s{_sppPageToken = a});

-- | The maximum number of items to return with this call.
sppPageSize :: Lens' ScanProvisionedProducts (Maybe Natural)
sppPageSize = lens _sppPageSize (\ s a -> s{_sppPageSize = a}) . mapping _Nat;

instance AWSRequest ScanProvisionedProducts where
        type Rs ScanProvisionedProducts =
             ScanProvisionedProductsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ScanProvisionedProductsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProducts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ScanProvisionedProducts where

instance NFData ScanProvisionedProducts where

instance ToHeaders ScanProvisionedProducts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ScanProvisionedProducts"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ScanProvisionedProducts where
        toJSON ScanProvisionedProducts'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _sppAcceptLanguage,
                  ("AccessLevelFilter" .=) <$> _sppAccessLevelFilter,
                  ("PageToken" .=) <$> _sppPageToken,
                  ("PageSize" .=) <$> _sppPageSize])

instance ToPath ScanProvisionedProducts where
        toPath = const "/"

instance ToQuery ScanProvisionedProducts where
        toQuery = const mempty

-- | /See:/ 'scanProvisionedProductsResponse' smart constructor.
data ScanProvisionedProductsResponse = ScanProvisionedProductsResponse'
  { _spprsNextPageToken       :: !(Maybe Text)
  , _spprsProvisionedProducts :: !(Maybe [ProvisionedProductDetail])
  , _spprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScanProvisionedProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'spprsProvisionedProducts' - Information about the provisioned products.
--
-- * 'spprsResponseStatus' - -- | The response status code.
scanProvisionedProductsResponse
    :: Int -- ^ 'spprsResponseStatus'
    -> ScanProvisionedProductsResponse
scanProvisionedProductsResponse pResponseStatus_ =
  ScanProvisionedProductsResponse'
  { _spprsNextPageToken = Nothing
  , _spprsProvisionedProducts = Nothing
  , _spprsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
spprsNextPageToken :: Lens' ScanProvisionedProductsResponse (Maybe Text)
spprsNextPageToken = lens _spprsNextPageToken (\ s a -> s{_spprsNextPageToken = a});

-- | Information about the provisioned products.
spprsProvisionedProducts :: Lens' ScanProvisionedProductsResponse [ProvisionedProductDetail]
spprsProvisionedProducts = lens _spprsProvisionedProducts (\ s a -> s{_spprsProvisionedProducts = a}) . _Default . _Coerce;

-- | -- | The response status code.
spprsResponseStatus :: Lens' ScanProvisionedProductsResponse Int
spprsResponseStatus = lens _spprsResponseStatus (\ s a -> s{_spprsResponseStatus = a});

instance NFData ScanProvisionedProductsResponse where
