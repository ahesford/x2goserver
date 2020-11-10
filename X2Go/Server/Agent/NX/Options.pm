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

our @EXPORT_OK = qw (MODE_INVALID MODE_ADD_UPDATE MODE_REMOVE
                     parse_options interpret_transform transform_intermediate intermediate_to_string compact_intermediate);


# These are actually supposed to be enums, but since Perl doesn't have a
# proper way of creating enums (at least not natively), we'll emulate that
# using small functions.
sub MODE_INVALID { return 0; }
sub MODE_ADD_UPDATE { return 1; }
sub MODE_REMOVE { return 2; }


# Accepts an options string and returns a reference to an array of hashes
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
    if (0 < scalar (@{$options})) {
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
# parameters (which, in turn, are the parameters this function expects).
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
      # though that's technically legit and would semantically fit the notion
      # as well, we rather want to represent this situation by an empty hash.
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
# Expects an intermediate options reference as its first parameter, a mode
# value which can be either MODE_REMOVE for removals or MODE_ADD_UPDATE for
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

  my $intermediate = shift;
  my $mode = shift;
  my $option = shift;

  if ('ARRAY' ne ref ($intermediate)) {
    print {*STDERR} 'Invalid options reference type passed (' . ref ($intermediate) . "), erroring out.\n";
    $error_detected = 1;
  }

  if (!($error_detected)) {
    if (0 < scalar (@{$intermediate})) {
      foreach my $entry (@{$intermediate}) {
        if (!defined ($entry)) {
          print {*STDERR} "Invalid options array passed, erroring out.\n";
          $error_detected = 1;
        }
      }
    }
  }

  if (!($error_detected)) {
    if (!(defined ($mode)) || (MODE_INVALID == $mode)) {
      print {*STDERR} "Invalid mode option passed, erroring out.\n";
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
    $ret = dclone ($intermediate);

    my $elements_left = @{$ret};

    if (MODE_REMOVE == $mode) {
      # Let the filter function handle the actual work.
      @{$ret} = grep { filter_option_remove ($work_option_key, $work_option_value, $_, --$elements_left) } @{$ret};

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
      ## no critic (BuiltinFunctions::ProhibitBooleanGrep)
      if (scalar (grep { filter_find_key ($work_option_key, $work_option_value, $_, --$elements_left) } @{$ret})) {
      ## critic (BuiltinFunctions::ProhibitBooleanGrep)
        # Such an option already exists, we'll modify all occurrences.
        $elements_left = @{$ret};
        $ret = [ map { filter_option_modify ($work_option_key, $work_option_value, $_, --$elements_left) } @{$ret} ];
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

  my $mode = MODE_INVALID;
  if (defined ($transform)) {
    $mode = MODE_ADD_UPDATE;
    my $sanitized_transform = $transform;

    # Check if non-empty, empty transform strings can only mean an
    # append/modify operation.
    if ($transform) {
      if (q{-} eq substr ($transform, 0, 1)) {
        # Option starts with a dash, so must be a removal operation.
        $mode = MODE_REMOVE;
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

# Compacts entries in the intermediate options array.
#
# Expects an intermediate options reference as its first and only parameter.
#
# Compacting means that:
#   - Duplicated keys are removed, only the last one is kept.
#   - Empty key-value pairs are discarded (unless it's the only element).
#
# Returns a reference to the modified, compacted intermediate options array.
#
# On error, returns a reference to undef.
sub compact_intermediate {
  my $ret = undef;
  my $error_detected = 0;
  my @new_intermediate = ();

  my $intermediate = shift;

  if ('ARRAY' ne ref ($intermediate)) {
    print {*STDERR} 'Invalid options reference type passed (' . ref ($intermediate) . "), erroring out.\n";
    $error_detected = 1;
  }

  if (!($error_detected)) {
    if (0 < scalar (@{$intermediate})) {
      foreach my $entry (@{$intermediate}) {
        if (!defined ($entry)) {
          print {*STDERR} "Invalid options array passed, erroring out.\n";
          $error_detected = 1;
        }
      }
    }
  }

  if (!($error_detected)) {
    # First, save display number part.
    my $display_number = pop (@{$intermediate});

    # Here's the clever part:
    #   - Copy data into a single hash.
    #     This will not preserve the order, but make sure that each entry is
    #     unique.
    #   - To preserve the order, evict entries from the original intermediate
    #     array iff this temporary hash already contains such a key.
    #     This makes sure that the original intermediate array will only
    #     contain unique entries in the right order - at least if the order is
    #     "first seen". Implementing this in a "last seen" manner would be a
    #     lot more complicated.
    my %temp_hash = ();
    @{$intermediate} = grep {
      my $grep_ret = 0;

      # This foreach loop will only execute at most once, really, since each
      # hash in an intermediate array is supposed to contain just one element.
      #
      # Additionally, it might not execute at all if the hash is empty, which
      # will implicitly remove the empty element through our $grep_ret's
      # initial value.
      #
      # Do not use each () here. It doesn't shorten the code and is very
      # sensitive to the internal iterator. Additionally, it makes
      # modifications unsafe. While we don't need that right now, it's
      # probably a good idea to keep this future-proof.
      foreach my $key (keys (%{$_})) {
        my $value = $_->{$key};

        # If the key exists in the temporary hash, this element is a duplicate
        # and we can mark it for deletion.
        # Otherwise, we'll have to keep it.
        if (!(exists ($temp_hash{$key}))) {
          $grep_ret = 1;
        }

        # And in any case, update the value (or create a new entry).
        $temp_hash{$key} = $value;
      }

      $grep_ret;
    } @{$intermediate};

    # Lastly, map values from the temporary hash back to the intermediate
    # array.
    foreach my $entry (@{$intermediate}) {
      foreach my $key (keys (%{$entry})) {
        $entry->{$key} = $temp_hash{$key};
      }
    }

    # We need to add an empty element if the intermediate array is now empty.
    if (0 == scalar (@{$intermediate})) {
      print {*STDERR} "Compacting operation led to option string being empty, adding empty element though deprecated.\n";
      push (@{$intermediate}, { });
    }

    # Lastly, re-add the display number part.
    push (@{$intermediate}, $display_number);

    $ret = $intermediate;
  }

  return $ret;
}

1;

__END__

=pod

=head1 NAME

X2Go::Server::Agent::NX::Options - NX Options modification module

=head1 SYNOPSIS

 use X2Go::Server::Agent::NX::Options;

 # Options string, probably read in from somewhere, but
 # hardcoded here.
 my $options = 'some=option,another=opt,more=values,some=override,more=data:90';

 # Parse into an intermediate form.
 my $intermediate = X2Go::Server::Agent::NX::Options::parse_options ($options);

 # Check for errors.
 die "Unable to parse option string, aborting.\n" unless (defined ($intermediate));

 # (Optionally) compact it, this should make the duplicated "some" and "more"
 # keys unique.
 $intermediate = X2Go::Server::Agent::NX::Options::compact_intermediate ($intermediate);

 # Error handling ...
 die "Unable to compact intermediate array, aborting.\n" unless (defined ($intermediate));

 # Add to options string.
 my $transform_op = '+new=value';

 # Parse and sanitize transform string.
 my $interpreted_transform_ref = X2Go::Server::Agent::NX::Options::interpret_transform ($transform_op);

 # Error handling ...
 die "Invalid transformation passed, aborting.\n" unless (defined ($interpreted_transform_ref));

 # Extract transformation data.
 my ($transform_mode, $sanitized_transform) = @{$interpreted_transform_ref};

 # Apply transformation.
 $intermediate = X2Go::Server::Agent::NX::Options::transform_intermediate ($intermediate, $transform_mode, $sanitized_transform);

 # Error handling ...
 die "Error while transforming intermediate representation, aborting.\n" unless (defined ($intermediate));

 # Try to remove a combination which doesn't exist, this should not modify the
 # intermediate.
 # No more comments for things that were already explained.
 $transform_op = '-another=doesnotexist';
 $interpreted_transform_ref = X2Go::Server::Agent::NX::Options::interpret_transform ($transform_op);
 die "Invalid transformation passed, aborting.\n" unless (defined ($interpreted_transform_ref));
 ($transform_mode, $sanitized_transform) = @{$interpreted_transform_ref};
 $intermediate = X2Go::Server::Agent::NX::Options::transform_intermediate ($intermediate, $transform_mode, $sanitized_transform);
 die "Error while transforming intermediate representation, aborting.\n" unless (defined ($intermediate));

 # Remove a key unconditionally, this should change the intermediate.
 $transform_op = '-some';
 $interpreted_transform_ref = X2Go::Server::Agent::NX::Options::interpret_transform ($transform_op);
 die "Invalid transformation passed, aborting.\n" unless (defined ($interpreted_transform_ref));
 ($transform_mode, $sanitized_transform) = @{$interpreted_transform_ref};
 $intermediate = X2Go::Server::Agent::NX::Options::transform_intermediate ($intermediate, $transform_mode, $sanitized_transform);
 die "Error while transforming intermediate representation, aborting.\n" unless (defined ($intermediate));

 # Modify/update a key.
 $transform_op = '+another=newval';
 $interpreted_transform_ref = X2Go::Server::Agent::NX::Options::interpret_transform ($transform_op);
 die "Invalid transformation passed, aborting.\n" unless (defined ($interpreted_transform_ref));
 ($transform_mode, $sanitized_transform) = @{$interpreted_transform_ref};
 $intermediate = X2Go::Server::Agent::NX::Options::transform_intermediate ($intermediate, $transform_mode, $sanitized_transform);
 die "Error while transforming intermediate representation, aborting.\n" unless (defined ($intermediate));

 # Transform back into a string.
 my $out = X2Go::Server::Agent::NX::Options::intermediate_to_string ($intermediate);

 # Error handling ...
 die "Unable to transform intermediate back into string, aborting.\n" unless (defined ($out));

 # At this point, $out should be 'another=newval,more=data,new=value:90'.

=head1 DESCRIPTION

Use this module to modify or extract data from B<X2Go/NX Agent> options
strings.
Refer to L</OPTIONS STRINGS> for an in-depth description of options strings.

First, transform the input options string into an intermediate representation
via C<parse_options>.
The options string I<must> end with a display specification (i.e., it must end
in C<:displaynumber>).
Parsing errors are indicated by it returning C<undef>.
The returned value is actually a I<reference> to an I<array> of I<hash>
I<references>, but you should make no assumptions to the layout or even its
actual format.
Treat it like a black box.
Crucially, whenever an I<intermediate> is expected, such a I<reference> should
be passed.

To remove redundant or empty entries within an options string, pass the
I<intermediate> to C<compact_intermediate>.
This is entirely optional and can be done at any step, as long as an
I<intermediate> is available.

To parse transformations, pass each one to C<interpret_transform>.
Refer to L</TRANSFORMATIONS> for documentation on transformation formats.
This will either return C<undef> on error, or an array of two scalars - the
transformation mode (an internal number) and the sanitized transformation
string (i.e., the original transformation string with any preceding operator
removed).

Pass the I<intermediate>, the I<transformation mode> and the
I<sanitized transformation string> to C<transform_intermediate> to modify the
intermediate value.

Repeat this until the I<intermediate> is modified to your liking.

Finally, pass the I<intermediate> to C<intermediate_to_string> in order to
parse it back into a normal string.
This operation is essentially the opposite of C<parse_options>.
As usual, C<undef> is returned on error.

Generally, parsing an options string to an intermediate via C<parse_options>
and then immediately parsing it back into a string via
C<intermediate_to_string> I<should> always produce an options string that is
identical to the original options string (assuming no errors occurred).

If this is not the case, please report a bug.

=head1 OPTIONS STRINGS

B<X2Go/NX Agent> options strings are fully documented in
L<nxagent's documentation|nxagent(1)> and additional, linked places therein.

This module is not really concerned with the actual content of an options
string, but mostly its format.

An options string follows the form
[[I<key>[C<=>I<value>,]]C<:>I<displaynumber>.

This has some interesting implications:

=over 4

=item *

Key-value pairs are entirely optional.
For example, an options string like C<:42> is well-formed.

=item *

Options strings I<must> end with a display number separator, i.e., a B<colon>
(C<:>) and a display number.

No parsing is done on the display number, so any string (even the empty
string) is valid as far as this module is concerned.

The display number, however, I<can not> contain a B<colon> (C<:>), since that
would make it a new display number separator.

This module will parse the options string from right to left when searching
for the display number separator and use the first one it can find.

=item *

Key-value pairs are separated via a B<comma> (C<,>).
Hence, this character is not valid in neither keys nor values.
As a workaround, it can be URL-encoded, as, e.g., C<%2C>, and then used as
part of either keys or values.

=item *

Key-value pairs can be empty.
This is supported and empty key-value pairs will be preserved, but will
trigger warnings at parse time.

An options string such as C<,,,:65> is hence valid.

To remove such empty elements, use C<compact_intermediate>.
An implicit empty element is added whenever the resulting options string would
only contain the display number.
This one I<can not> be removed, but also won't show up anywhere.
Adding any non-empty new key will automatically replace such an empty element,
without any need for actual compactation.

=item *

In a key-value pair, keys and values are separated from each other via an
B<equal sign> (C<=>).

Hence, this character is not a valid part of a key.
It can, however, be a valid part of a value.

To use this character as part of a key, it can be URL-encoded as, e.g.,
C<%3D>.

While it is legal as part of a value, it is recommended to also URL-encode it
when used as part of a value in order to not confuse other parsers.

An options string such as C<this%3Dis%3Da=key:38> is just as valid as
C<this=is=a=key:38> or C<this=is%3Da%3Dkey:38>.

However, the semantics differ.
While the latter two key-value pairs are logically equivalent to each other,
they are very much different from the first one.

For the first case, the I<key> will be C<this%3Dis%3Da> (or, logically, also
C<this=is=a>, which can not be directly represented), while the I<value> will
be just C<key>.

The latter two will parse into a I<key> C<this> with a I<value> of C<is=a=key>
(or, logically equivalent, C<is%3Da%3Dkey>).

=item *

Quoting with any character is unsupported.
Quotes and other grouping characters (like B<curly braces> [C<{}>]) are seen
verbatim without any special meaning.

=item *

Since options strings are typically parsed by C applications, C<NUL> (control)
characters are prematurely terminating the string and hence cannot be directly
embedded.
Indirectly, they can be embedded by URL-encoding them as C<%00>.

There is, however, no guarantee that an application unpacking such a string
will be able to scan any data after the first embedded C<NUL> character.

It is highly recommended to avoid using embedded C<NUL> characters.

This module will not explicitly scan for them, and, hence, also not issue warnings
related to those characters.

=item *

There are no provisions (other than the mentioned invalid characters) on the
content of keys and values.

Importantly, this also means that the same key can show up multiple times in
the options string.
Semantically, this is redundant, since only the last occurrence of a key
(assuming the options string is parsed from left to right) will take any
effect.
Syntactically, it's completely legal, though.

It is recommended to avoid duplicate keys in the input options string.

Note that, due to the nature of the supported transformations, keys can not be
duplicated with this module.

To remove duplicated keys, use C<compact_intermediate>.
This will preserve the order in a first-seen fashion.

=item *

A key-value pair with an empty key but a non-empty value is allowed.

Likewise, a key-value pair with a non-empty key, but an empty value is
allowed.
In this case, the value will be interpreted as an empty string in order to
differentiate it from a non-existent value.

=back

=head1 TRANSFORMATIONS

Transformations follow the form [B<+>]|B<->I<key>[B<=>I<value>], which means
that:

=over 4

=item *

They can be prefixed with a B<plus> character (C<+>) to indicate either
additions or modifications.
A missing prefix character is interpreted like a B<plus> character.

If the given I<key> already exists in the intermediate, the key-value pair
will be updated with the provided I<value> (if any), or a new key-value pair
added.

Insertions always take place at the end of the intermediate.

The I<value> can be omitted, in which case I<key> will be added without a
value on insertions or a potentially existing value removed on updates.

=item *

If they are prefixed with a B<minus> character (C<->), deletion mode is
activated.

If the given I<key> is not part of the intermediate, no deletion will occur.

Otherwise, the optional I<value> determines deletion: if no value has been
provided, I<key> will be removed from the intermediate regardless of its
value.
If the optional I<value> has been provided, I<key> will only be removed if
both values match.

=back

=head1 AUTHOR

This manual has been written by
Mihai Moldovan L<E<lt>ionic@ionic.deE<gt>|mailto:ionic@ionic.de> for the X2Go
project (L<https://www.x2go.org>).

=cut
