{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Not used right now.. need to figure out how to make this generic for use in React.Flux for example.
--

module LN.UI.Core.Access (
  isMemberOfOrganization,
  isMemberOfOrganizationHTML,
  isMemberOfOrganizationHTML',
  permissionsHTML,
  permissionsHTML',
  permCreateEmpty,
  permReadEmpty,
  permUpdateEmpty,
  permDeleteEmpty,
  permExecuteEmpty,
  permissionsMatchCreateHTML,
  permissionsMatchReadHTML,
  permissionsMatchUpdateHTML,
  permissionsMatchDeleteHTML,
  permissionsMatchExecuteHTML,
  permissionsMatchHTML
) where



import           LN.T
import           LN.UI.Core.Types



-- | Check whether a user is a member of Team_Members
--
isMemberOfOrganization :: OrganizationPackResponse -> Bool
isMemberOfOrganization OrganizationPackResponse{..} = Team_Members `elem` organizationPackResponseTeams



-- | Supports handlers for if a user is/isnt a member
--
isMemberOfOrganizationHTML
  :: OrganizationPackResponse
  -> View_
  -> View_
  -> View_

isMemberOfOrganizationHTML pack is_member_handler isnt_member_handler =
  if isMemberOfOrganization pack
    then is_member_handler
    else isnt_member_handler



-- | Supports a handler if a user is a member
--
isMemberOfOrganizationHTML'
  :: OrganizationPackResponse
  -> View_
  -> View_

isMemberOfOrganizationHTML' pack is_member_handler =
  if isMemberOfOrganization pack
    then is_member_handler
    else mempty



permissionsHTML
  :: Permissions
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
permissionsHTML perms create_cb no_create_cb read_cb no_read_cb update_cb no_update_cb delete_cb no_delete_cb execute_cb no_execute_cb =
  mconcat
    [
      if Perm_Create  `elem` perms then create_cb  else no_create_cb
    , if Perm_Read    `elem` perms then read_cb    else no_read_cb
    , if Perm_Update  `elem` perms then update_cb  else no_update_cb
    , if Perm_Delete  `elem` perms then delete_cb  else no_delete_cb
    , if Perm_Execute `elem` perms then execute_cb else no_execute_cb
    ]



permissionsHTML'
  :: Permissions
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
  -> View_
permissionsHTML' perms create_cb read_cb update_cb delete_cb execute_cb =
  permissionsHTML perms create_cb mempty read_cb mempty update_cb mempty delete_cb mempty execute_cb mempty



permCreateEmpty, permReadEmpty, permUpdateEmpty, permDeleteEmpty, permExecuteEmpty :: View_
permCreateEmpty  = mempty
permReadEmpty    = mempty
permUpdateEmpty  = mempty
permDeleteEmpty  = mempty
permExecuteEmpty = mempty



permissionsMatchCreateHTML :: Permissions -> View_ -> View_ -> View_
permissionsMatchCreateHTML = permissionsMatchHTML Perm_Create



permissionsMatchReadHTML :: Permissions -> View_ -> View_ -> View_
permissionsMatchReadHTML = permissionsMatchHTML Perm_Read



permissionsMatchUpdateHTML :: Permissions -> View_ -> View_ -> View_
permissionsMatchUpdateHTML = permissionsMatchHTML Perm_Update



permissionsMatchDeleteHTML :: Permissions -> View_ -> View_ -> View_
permissionsMatchDeleteHTML = permissionsMatchHTML Perm_Delete



permissionsMatchExecuteHTML :: Permissions -> View_ -> View_ -> View_
permissionsMatchExecuteHTML = permissionsMatchHTML Perm_Execute



permissionsMatchHTML
  :: Permission
  -> Permissions
  -> View_
  -> View_
  -> View_
permissionsMatchHTML perm_to_match permissions is_match_handler isnt_match_handler =
  if perm_to_match `elem` permissions then is_match_handler else isnt_match_handler
