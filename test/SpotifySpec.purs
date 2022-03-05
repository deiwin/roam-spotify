module Test.SpotifySpec where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Spotify (timestamp, PlaybackState(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "timestamp" do
    it "uses HH:MM:SS format" do
      Milliseconds 0.0
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "00:00:00")
    it "calcultes seconds" do
      Milliseconds 5000.0
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "00:00:05")
    it "rounds seconds up to nearest int" do
      Milliseconds 4999.0
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "00:00:05")
    it "rounds seconds down to nearest int" do
      Milliseconds 5001.0
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "00:00:05")
    it "flows seconds over to minutes" do
      Milliseconds (61.0 * 1000.0)
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "00:01:01")
    it "flows minutes over to hours" do
      Milliseconds (61.0 * 60.0 * 1000.0)
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "01:01:00")
    it "doesn't overflow hours" do
      Milliseconds (25.0 * 60.0 * 60.0 * 1000.0)
        # createPlaybackState
        # timestamp
        # (_ `shouldEqual` "25:00:00")

createPlaybackState :: Milliseconds -> PlaybackState
createPlaybackState progress = PlaybackState { progress, isPlaying: true }
