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
-- Module      : Network.AWS.Inspector.ListFindings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists findings generated by the assessment run specified by the run
-- ARNs.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_ListFindings.html AWS API Reference> for ListFindings.
module Network.AWS.Inspector.ListFindings
    (
    -- * Creating a Request
      listFindings
    , ListFindings
    -- * Request Lenses
    , lfRunARNs
    , lfNextToken
    , lfFilter
    , lfMaxResults

    -- * Destructuring the Response
    , listFindingsResponse
    , ListFindingsResponse
    -- * Response Lenses
    , lfrsNextToken
    , lfrsFindingARNList
    , lfrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listFindings' smart constructor.
data ListFindings = ListFindings'
    { _lfRunARNs    :: !(Maybe [Text])
    , _lfNextToken  :: !(Maybe Text)
    , _lfFilter     :: !(Maybe FindingsFilter)
    , _lfMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfRunARNs'
--
-- * 'lfNextToken'
--
-- * 'lfFilter'
--
-- * 'lfMaxResults'
listFindings
    :: ListFindings
listFindings =
    ListFindings'
    { _lfRunARNs = Nothing
    , _lfNextToken = Nothing
    , _lfFilter = Nothing
    , _lfMaxResults = Nothing
    }

-- | The ARNs of the assessment runs that generate the findings that you want
-- to list.
lfRunARNs :: Lens' ListFindings [Text]
lfRunARNs = lens _lfRunARNs (\ s a -> s{_lfRunARNs = a}) . _Default . _Coerce;

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the __ListFindings__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from previous response to continue
-- listing data.
lfNextToken :: Lens' ListFindings (Maybe Text)
lfNextToken = lens _lfNextToken (\ s a -> s{_lfNextToken = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
lfFilter :: Lens' ListFindings (Maybe FindingsFilter)
lfFilter = lens _lfFilter (\ s a -> s{_lfFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
lfMaxResults :: Lens' ListFindings (Maybe Int)
lfMaxResults = lens _lfMaxResults (\ s a -> s{_lfMaxResults = a});

instance AWSRequest ListFindings where
        type Rs ListFindings = ListFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListFindingsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "findingArnList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListFindings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListFindings" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListFindings where
        toJSON ListFindings'{..}
          = object
              (catMaybes
                 [("runArns" .=) <$> _lfRunARNs,
                  ("nextToken" .=) <$> _lfNextToken,
                  ("filter" .=) <$> _lfFilter,
                  ("maxResults" .=) <$> _lfMaxResults])

instance ToPath ListFindings where
        toPath = const "/"

instance ToQuery ListFindings where
        toQuery = const mempty

-- | /See:/ 'listFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
    { _lfrsNextToken      :: !(Maybe Text)
    , _lfrsFindingARNList :: !(Maybe [Text])
    , _lfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextToken'
--
-- * 'lfrsFindingARNList'
--
-- * 'lfrsResponseStatus'
listFindingsResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFindingsResponse
listFindingsResponse pResponseStatus_ =
    ListFindingsResponse'
    { _lfrsNextToken = Nothing
    , _lfrsFindingARNList = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
lfrsNextToken :: Lens' ListFindingsResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\ s a -> s{_lfrsNextToken = a});

-- | A list of ARNs specifying the findings returned by the action.
lfrsFindingARNList :: Lens' ListFindingsResponse [Text]
lfrsFindingARNList = lens _lfrsFindingARNList (\ s a -> s{_lfrsFindingARNList = a}) . _Default . _Coerce;

-- | The response status code.
lfrsResponseStatus :: Lens' ListFindingsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a});