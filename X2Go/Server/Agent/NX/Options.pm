# Copyright (C) 2018-2020 X2Go Project - https://wiki.x2go.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

package X2Go::Server::Agent::NX::Options;

# Disable some Perl::Critic violations.
## no critic (ProhibitParensWithBuiltins)

use strict;
use warnings;

use base 'Exporter';
use English qw (-no_match_vars);
use Storable qw (dclone);

our @EXPORT_OK = qw (parse_options interpret_transform transform_intermediate intermediate_to_string);

# Accepts an option string and returns a reference to an array of hashes
# (actually hash references) corresponding to the parsed key-value pairs.
#
# Empty components are allowed, but will issue a warning message. In such a
# case, the hash at the corresponding array position will be empty.
#
# Keys that do not have a value assigned will be given a value of "undef" in
# order to be able to distinguish them from keys with an empty string as their
# value.
#
# Caveat: the last component will be split from the port definition. DO NOT
# SIMPLY JOIN THE LIST OR YOU WILL ADD A TRAILING COMMA! The port component
# hash retains the colon separator.
#
# On error, returns an undef reference.
sub parse_options {
  my $ret = undef;
  my @intermediate = ();
  my $error_detected = 0;
  my $options = shift;
  my $next_discard = shift;

  if (defined ($next_discard)) {
    print {*STDERR} "Multiple arguments passed in, all but the first one are ignored!\n";
  }

  if (!(defined ($options))) {
    print {*STDERR} "No argument provided for options string, returning undef.\n";
    $error_detected = 1;
  }

  if (!($error_detected)) {
    my @components = split (/,/sxm, $options, -1);
    foreach my $option (@components) {
      # We use undef to denote that some component was not provided at all
      # to disambiguate non-provided and empty strings.
      my ($key, $value) = (undef, undef);
      my %kv_hash = ();

      my @kv = split (/=/sxm, $option, 2);

      if (1 > scalar (@kv)) {
        print {*STDERR} "Options string has empty component, this is deprecated. Adding empty element.\n";

        push (@intermediate, \%kv_hash);
      }
      elsif (3 <= scalar (@kv)) {
        print {*STDERR} "Options string has three or more components, this is a bug in $PROGRAM_NAME. Erroring out.\n";
        $error_detected = 1;
        last;
      }
      else {
        $key = shift (@kv);
        $value = shift (@kv);

        # Add to hash - every hash will contain a single key-value pair only.
        $kv_hash{$key} = $value;

        # Then add the hash as an entry in our return array - by reference.
        push (@intermediate, \%kv_hash);
      }
    }

    # Special handling for the last option, which does not use a comma as the
    # delimiter but a colon.
    #
    # Note that it can either be part of the key or the value.
    if (!($error_detected)) {
      if ((0 < scalar (@intermediate)) && (defined ($intermediate[0]))) {
        my $last_hash_ref = pop (@intermediate);
        my $hash_count = 0;
        my $last_component = q{};
        my $in_value = 0;
        my $last_component_key = undef;

        # Fetch last component and check for sanity.
        # An empty hash is implicitly handled by initializing $last_component to
        # an empty string, which will fail the splitting later on.
        foreach my $key (keys %{$last_hash_ref}) {
          ++$hash_count;

          if (1 < $hash_count) {
            print {*STDERR} "More than one element found in last element's hash, this is a bug in $PROGRAM_NAME. Ignoring subsequent entries.\n";
            last;
          }

          $last_component = $last_component_key = $key;

          if (defined ($last_hash_ref->{$key})) {
            # If a value exists, the display specifier can not be part of the
            # key.
            $in_value = 1;
            $last_component = $last_hash_ref->{$key};
          }
        }

        # Don't use split() here. While we could use a more or less complex
        # regex to extract the last(!) port specifier only, this would render
        # the LIMIT parameter to split() useless (since additional capture
        # groups are not part of the limit).
        # Thus going the manual route here.
        my $last_pos = rindex ($last_component, q{:});

        if ($[ > $last_pos) {
          print {*STDERR} "No display port seperator found in the options string. Erroring out.\n";
          $error_detected = 1;
        }
        else {
          my $last_component_left = substr ($last_component, 0, $last_pos);
          my $last_component_right = substr ($last_component, $last_pos);

          my %last_component_hash = ();

          if ($in_value) {
            $last_component_hash{$last_component_key} = $last_component_left;
          }
          else {
            # Sanity check on the key. If it's empty, issue a warning and don't
            # use it.
            if (0 == length ($last_component_left)) {
              print {*STDERR} "Options string has empty component, this is deprecated. Adding empty element.\n";
            }
            else {
              $last_component_hash{$last_component_left} = undef;
            }
          }

          # Now add the last component hash to the array again.
          push (@intermediate, \%last_component_hash);

          # Prepare a new hash object, with the key set to the display port part
          # and value to undef to mark it invalid.
          my %display_port_hash = ();
          $display_port_hash{$last_component_right} = undef;

          # Add this to the return value as well.
          push (@intermediate, \%display_port_hash);
        }
      }
    }
  }

  if (!($error_detected)) {
    $ret = \@intermediate;
  }

  return $ret;
}

# Takes an intermediate options string representation array reference(!) and
# returns a string.
# This is essentially the opposite of parse_options.
# Parsing an options string and passing the result through this function again
# SHOULD (if initial options string has been a valid one to begin with) yield
# the initial options string again.
# On error, returns undef.
sub intermediate_to_string {
  my $ret = undef;
  my $error_detected = 0;

  my $options = shift;

  if ('ARRAY' ne ref ($options)) {
    print {*STDERR} 'Invalid options reference type passed (' . ref ($options) . "), returning undef.\n";
    $error_detected = 1;
  }

  if (!($error_detected)) {
    if (1 == scalar (@{$options})) {
      foreach my $entry (@{$options}) {
        if (!defined ($entry)) {
          print {*STDERR} "Invalid options array passed, returning undef.\n";
          $error_detected = 1;
        }
      }
    }
  }

  if (!($error_detected)) {
    # Last entry should contain the display port part only.
    # We can detect it through counting.
    my $elements_left = @{$options};

    # Handle entries iteratively, merging them into one string.
    foreach my $entry (@{$options}) {
      --$elements_left;

      if (!defined ($entry)) {
        print {*STDERR} "Invalid options entry encountered, returning undef.\n";
        $error_detected = 1;
        last;
      }

      if ('HASH' ne ref ($entry)) {
        print {*STDERR} 'Entry in array has invalid type (' . ref ($entry) ."), returning undef.\n";
        $error_detected = 1;
        last;
      }

      if (1 < scalar (keys (%{$entry}))) {
        print {*STDERR} "More than one entry encountered in hash of current element, returning undef.\n";
        $error_detected = 1;
        last;
      }

      # Must be either empty or have one element, so... go for it.
      if (0 == scalar (keys (%{$entry}))) {
        if (0 != $elements_left) {
          if (defined ($ret)) {
            $ret .= q{,};
          }
          else {
            # Mark first entry as empty. Don't remove this, or else.
            $ret = q{};
          }
        }
        else {
          # Special handling for last element, which is always supposed to
          # contain the display port.
          print {*STDERR} "No entry found in display port hash, returning undef.\n";
          $error_detected = 1;
          last;
        }
      }
      else {
        # This foreach loop may look weird because, at that point, we know that
        # the hash contains one key exactly, but it's still an elegant way to
        # fetch the key and pseudo-iterate over it.
        foreach my $key (keys (%{$entry})) {
          my $tmp = $key;

          if (0 != $elements_left) {
            if (defined ($entry->{$key})) {
              $tmp .= q{=} . $entry->{$key};
            }
          }

          if (defined ($ret)) {
            if (0 != $elements_left) {
              $ret = join (q{,}, ($ret, $tmp));
            }
            else {
              $ret .= $tmp;
            }
          }
          else {
            $ret = $tmp;
          }
        }
      }
    }
  }

  if ($error_detected) {
    $ret = undef;
  }

  return $ret;
}

# Helper function that checks for errors in options passed as filter
# parameters (which, in turn, are the parameter this function expects).
#
# Returns true if all checks passed, false otherwise.
sub sanitize_input_filter {
  my $ret = 1;

  my $work_option_key = shift;
  my $work_option_value = shift;
  my $cur_option = shift;
  my $elems_left = shift;

  if (!((defined ($work_option_key)) && (defined ($cur_option)) && (defined ($elems_left)))) {
    print {*STDERR} "Invalid options passed to filter, keeping entry.\n";
    $ret = 0;
  }

  if ($ret) {
    if ('HASH' ne ref ($cur_option)) {
      print {*STDERR} "Option passed to filter is not a hash reference, keeping entry.\n";
      $ret = 0;
    }
  }

  if ($ret) {
    if (1 < scalar (keys (%{$cur_option}))) {
      print {*STDERR} "Option passed to filter has more than one entry in hash, keeping entry.\n";
      $ret = 0;
    }
  }

  return $ret;
}

# Helper function that splits up the working option into a key and a value.
#
# Expects the working option as its only parameter.
#
# Returns a reference to an array with two entries - the key and the value.
# Caveat: the key cannot be undef (it's set to the empty string if it would
# be), but the value can be undef.
#
# In case of errors, returns a reference to undef.
sub sanitize_workoption_filter {
  my $ret = undef;
  my $error_detected = 0;

  my $working_option = shift;

  if (defined ($working_option)) {
    my $work_key = undef;
    my $work_value = undef;
    my @work_kv = split (/=/smx, $working_option, 2);

    if (2 < scalar (@work_kv)) {
      print {*STDERR} "Option-to-be-acted-upon string in filter has three or more components, this is a bug in $PROGRAM_NAME. Returning error.\n";
      $error_detected = 1;
    }

    if (!($error_detected)) {
      $work_key = shift (@work_kv);

      # Key can be undef if splitting failed, e.g., due to an empty input
      # string. We don't consider this an error, so reset the key to an empty
      # string.
      if (!(defined ($work_key))) {
        $work_key = q{};
      }

      $work_value = shift (@work_kv);

      $ret = [ $work_key, $work_value ];
    }
  }

  return $ret;
}

# Helper for a grep operation on the intermediate options array.
#
# Takes the option-to-remove's key and value, the current element and amount
# of elements left in the array as arguments and returns true if the element is
# not to be removed, false otherwise.
sub filter_option_remove {
  my $ret = 1;
  my $skip = 0;
  my $to_remove_key = shift;
  my $to_remove_value = shift;
  my $cur_option = shift;
  my $elems_left = shift;

  $skip = ((!(sanitize_input_filter ($to_remove_key, $to_remove_value, $cur_option, $elems_left)))
           || (0 == $elems_left));

  if (!($skip)) {
    my $option_key = q{};
    my $option_value = undef;

    foreach my $tmp_option_key (keys (%{$cur_option})) {
      $option_key = $tmp_option_key;
      $option_value = $cur_option->{$tmp_option_key};
    }

    if ($to_remove_key eq $option_key) {
      # Okay, we've got a match. But we might have to also check the value...
      if (defined ($to_remove_value)) {
        # Yep, value must match, too, but beware of undef values in the current
        # option entry.
        if ((defined ($option_value)) && ($to_remove_value eq $option_value)) {
          # Everything matches, mark for removal.
          $ret = 0;
        }
      }
      else {
        $ret = 0;
      }
    }
  }

  return $ret;
}

# Helper for a grep operation on the intermediate options array.
#
# Takes the option-to-find's key and value, the current element and amount of
# elements left in the array as arguments and returns true if the element has
# the same as the option we search for, false otherwise.
sub filter_find_key {
  my $ret = 0;
  my $skip = 0;
  my $needle_key = shift;
  my $needle_value = shift;
  my $cur_option = shift;
  my $elems_left = shift;

  $skip = ((!(sanitize_input_filter ($needle_key, $needle_value, $cur_option, $elems_left)))
           || (0 == $elems_left));

  if (!($skip)) {
    # We don't care about the values this time around.

    my $option_key = q{};

    foreach my $tmp_option_key (keys (%{$cur_option})) {
      $option_key = $tmp_option_key;
    }

    if ($option_key eq $needle_key) {
      $ret = 1;
    }
  }

  return $ret;
}

# Helper for a map operation on the intermediate options array.
#
# Takes the option-to-modify's key and value, the current element and amount of
# elements left in the array as arguments and returns the modified element or
# the original one, if modification was not necessary.
sub filter_option_modify {
  my $skip = 0;
  my $needle_key = shift;
  my $needle_value = shift;
  my $cur_option = shift;
  my $elems_left = shift;

  my @ret = ( $cur_option );

  $skip = ((!(sanitize_input_filter ($needle_key, $needle_value, $cur_option, $elems_left)))
           || (0 == $elems_left));

  if (!($skip)) {
    my $option_key = q{};

    foreach my $tmp_option_key (keys (%{$cur_option})) {
      $option_key = $tmp_option_key;
    }

    if ($option_key eq $needle_key) {
      my $new_opt = { };

      # Don't add empty options as an empty string key with undef value; even
      # though that's technically legit we want to represent this situation by
      # an empty hash.
      if (!(($needle_key) || (defined ($needle_value)))) {
        print {*STDERR} "Empty option addition/modification requested, this is deprecated. Adding empty hash.\n";
      }
      else {
        $new_opt->{$needle_key} = $needle_value;
      }

      @ret = ( $new_opt );
    }
  }

  return @ret;
}

# Removes from, adds to or modifies an entry in the intermediate options array.
#
# Expects an intermediate options reference as its first parameter, a boolean
# value which should be set to true for removals or false for
# modifications/additions and the option-to-be-modified as a third parameter.
#
# For removals, the function behaves like this:
#   - If only a key is specified, removes any entry that matches this key,
#     regardless of its value.
#   - If both a key and a value are specified, only matching combinations will
#     be removed from the array. That is, if the array already contains such a
#     key with either no value or a different value, it will be unaffected.
#
# Additions or modifications are handled like this:
#   - If a given key is part of the intermediate representation, all such
#     occurrences will be replaced by the new value.
#   - Otherwise, the new value will be added at the end of the intermediate
#     representation.
#
# Returns a reference to a modified *copy* of the intermediate options array.
#
# On error, returns a reference to undef.
sub transform_intermediate {
  my $ret = undef;
  my $error_detected = 0;

  my $options = shift;
  my $remove = shift;
  my $option = shift;

  if ('ARRAY' ne ref ($options)) {
    print {*STDERR} 'Invalid options reference type passed (' . ref ($options) . "), erroring out.\n";
    $error_detected = 1;
  }

  if (!($error_detected)) {
    if (1 == scalar (@{$options})) {
      foreach my $entry (@{$options}) {
        if (!defined ($entry)) {
          print {*STDERR} "Invalid options array passed, erroring out.\n";
          $error_detected = 1;
        }
      }
    }
  }

  if (!($error_detected)) {
    if (!(defined ($remove))) {
      print {*STDERR} "Invalid mode option boolean passed, erroring out.\n";
      $error_detected = 1;
    }
  }

  if (!($error_detected)) {
    if (!(defined ($option))) {
      print {*STDERR} "No or invalid new option passed, erroring out.\n";
      $error_detected = 1;
    }
  }

  my $work_option_key = undef;
  my $work_option_value = undef;

  if (!($error_detected)) {
    my $work_opt_kv = sanitize_workoption_filter ($option);

    if (!(defined ($work_opt_kv))) {
      print {*STDERR} "Unable to split up working option into key and value pair, returning undef.\n";
      $error_detected = 1;
    }
    else {
      $work_option_key = shift (@{$work_opt_kv});
      $work_option_value = shift (@{$work_opt_kv});
    }
  }

  if (!($error_detected)) {
    # Set return value to a *deep copy* of our original array.
    $ret = dclone ($options);

    my $elements_left = @{$ret};

    if ($remove) {
      # Let the filter function handle the actual work.
      @{$ret} = grep { filter_option_remove ($work_option_key, $work_option_value, $_, --$elements_left) } (@{$ret});

      # Check to see if the intermediate representation is empty now (save for
      # the display port entry) and add an empty element if it is.
      if (1 == scalar (@{$ret})) {
        print {*STDERR} "Removal operation led to option string being empty, adding empty element though deprecated.\n";
        unshift (@{$ret}, { });
      }
    }
    else {
      # Yes, grep () isn't a great choice for a boolean comparison. It will do
      # what we need just fine, but doesn't short-circuit after finding the
      # first match, hence uselessly continuing through the full array.
      # List::MoreUtils::any would be more appropriate here, but this would add
      # another dependency and option strings are pretty small, so don't
      # overoptimize here.
      if (scalar (grep { filter_find_key ($work_option_key, $work_option_value, $_, --$elements_left) } (@{$ret}))) {
        # Such an option already exists, we'll modify all occurrences.
        $elements_left = @{$ret};
        $ret = [ map { filter_option_modify ($work_option_key, $work_option_value, $_, --$elements_left) } (@{$ret}) ];
      }
      else {
        my $new_opt = { $work_option_key => $work_option_value };

        # No such option exists, we'll add it to the end of the current
        # options.
        # At least in theory. Practically, there's one special case: if the
        # only "real" element is an empty one, for instance because the option
        # string was empty to begin with save the display port specifier, then
        # we want to replace this option instead.
        if ((2 == scalar (@{$ret})) && (!(scalar (keys (%{$ret->[0]}))))) {
          splice (@{$ret}, 0, 1, $new_opt);
        }
        else {
          splice (@{$ret}, -1, 0, $new_opt);
        }
      }
    }
  }

  return $ret;
}

# Helper function "interpreting" a transformation string.
#
# Takes the raw transformation string as its only parameter.
#
# Returns an array reference containing two elements: the transformation mode
# and a sanitized version of the transformation string, suitable for passing to
# transform_intermediate ().
#
# On error, returns undef.
sub interpret_transform {
  my $ret = undef;

  my $transform = shift;

  if (defined ($transform)) {
    my $mode = 0;
    my $sanitized_transform = $transform;

    # Check if non-empty, empty transform strings can only mean an
    # append/modify operation.
    if ($transform) {
      if (q{-} eq substr ($transform, 0, 1)) {
        # Option starts with a dash, so must be a removal operation.
        $mode = 1;
        $sanitized_transform = substr ($sanitized_transform, 1);
      }
      elsif ((q{+}) eq substr ($transform, 0, 1)) {
        # Options starting with a plus character are add/modify operations. The
        # default mode option here is fine, but we'll need to strip the initial
        # character.
        $sanitized_transform = substr ($sanitized_transform, 1);
      }

      # Everything else does not feature an explicit modifier, so we can take
      # the transformation string verbatim.
      # No need to actually do anything here, handled by the initialization.
    }

    # Set up return value accordingly.
    $ret = [ $mode, $sanitized_transform ];
  }

  return $ret;
}

1;
